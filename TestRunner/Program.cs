using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

class Program
{
    private static readonly string TestHeader = "test.h";
    private static readonly string TestsFile = "tests.txt";
    private static readonly string RuntimeFile = "runtime.c";
    private static readonly string ReportFile = "results.html";
    private static readonly string CompilerPath = "norcal/bin/Debug/norcal.exe";
    private static readonly string SimulatorPath = "sim6502/x64/Debug/sim6502.exe";
    private static readonly float TimeoutDuration = 5.0f;
    private static readonly bool ShowPassingTests = false;

    static void Main(string[] args)
    {
        // Load the tests from the tests file:
        List<Test> tests = new List<Test>();
        {
            Test test = new Test();
            foreach (string line in File.ReadAllLines(TestsFile))
            {
                string lineLower = line.ToLower();

                // Potentially finish the previous test:
                if (line.StartsWith("@") && test.Source.Trim() != "")
                {
                    tests.Add(test);
                    test = new Test();
                }

                // Collect metadata for the upcoming test:
                if (line.StartsWith("@ "))
                {
                    test.Description = line.Substring(2);
                }
                else if (lineLower.StartsWith("@out"))
                {
                    test.ExpectedOutput = ParseNumberList(line.Substring(4));
                }
                else if (lineLower.StartsWith("@error"))
                {
                    test.ExpectError = true;
                }
                else
                {
                    test.Source += line + "\n";
                }
            }

            // Finish the last test:
            if (test.Source.Trim() != "")
            {
                tests.Add(test);
            }
        }

        // Run the tests:
        int nextHtmlID = 0;
        object mutex = new object();
        List<Thread> threads = new List<Thread>();
        foreach (Test test in tests)
        {
            ThreadStart runTest = () =>
            {
                test.Passed = false;

                lock (mutex)
                {
                    test.HtmlID = string.Format("test{0}", nextHtmlID);
                    nextHtmlID += 1;
                }

                string sourceFile = test.HtmlID + ".c";
                string imageFile = test.HtmlID + ".nes";

                // Run the compiler:
                File.WriteAllText(sourceFile, test.Source);
                List<string> compilerArgs = new List<string>(
                    new[] { RuntimeFile, TestHeader, sourceFile, "-o", imageFile });
                Process process = RunProcess(CompilerPath, compilerArgs.ToArray());

                if (!process.HasExited)
                {
                    process.WaitForExit();
                    test.ActualError = "(compiler timed out)";
                    test.Passed = false;
                    goto Finished;
                }
                else if (process.ExitCode == 1)
                {
                    test.ActualError = "compiler error:<br>" + process.StandardError.ReadToEnd();
                    test.Passed = test.ExpectError;
                    goto Finished;
                }
                else if (process.ExitCode > 1)
                {
                    test.ActualError = "compiler panic:<br>" + process.StandardError.ReadToEnd();
                    test.Passed = false;
                    goto Finished;
                }

                // Run the compiled code:
                process = RunProcess(SimulatorPath, new[] { imageFile });
                if (!process.HasExited)
                {
                    process.Kill();
                    test.ActualError = "(simulator timed out)";
                    test.Passed = false;
                    goto Finished;
                }
                else if (process.ExitCode != 0)
                {
                    test.ActualError = "simulator error:<br>" + process.StandardError.ReadToEnd();
                    test.Passed = test.ExpectError;
                    goto Finished;
                }

                // Parse the output and check for correctness:
                int[] sim_output = ParseNumberList(process.StandardOutput.ReadToEnd());
                test.Cycles = sim_output.Last();
                test.ActualOutput = sim_output.Take(sim_output.Length - 1).ToArray();
                if (test.ExpectError)
                {
                    test.Passed = false;
                }
                else
                {
                    test.Passed = false;
                    if (test.ActualOutput.Length == test.ExpectedOutput.Length)
                    {
                        test.Passed = true;
                        for (int i = 0; i < test.ActualOutput.Length; i++)
                        {
                            if (test.ActualOutput[i] != test.ExpectedOutput[i])
                            {
                                test.Passed = false;
                                break;
                            }
                        }
                    }
                }

                Finished:
                File.Delete(sourceFile);
                File.Delete(imageFile);
                File.Delete(Path.ChangeExtension(imageFile, ".dbg"));
                Console.Write(".");
            };

            Thread thread = new Thread(runTest);
            thread.Start();
            threads.Add(thread);
        }

        // Wait for threads to finish:
        while (threads.Any(x => x.IsAlive))
        {
            Thread.Sleep(10);
        }

        // Generate the report:
        Maybe<string> firstErrorID = Maybe.Nothing;
        StringBuilder report = new StringBuilder();

        report.Append(HtmlHeader);
        int failures = tests.Where(x => !x.Passed).Count();
        if (failures == 0)
        {
            report.AppendFormat("<p class=\"success\">All {0} tests passed!</p>\n", tests.Count);
        }
        else
        {
            string plural = (failures > 1) ? "s" : "";
            report.AppendFormat("<p class=\"problem\">{0} test{1} failed.</p>\n", failures, plural);
        }
        report.Append(HtmlMiddle);
        foreach (Test test in tests.Where(x => !x.Passed || ShowPassingTests))
        {
            if (!test.Passed && !firstErrorID.HasValue)
            {
                firstErrorID = test.HtmlID;
            }
            report.Append("<tr>\n");
            if (test.Passed)
            {
                report.Append("<td bgcolor=\"#53F253\"></td>\n");
            }
            else
            {
                report.Append("<td bgcolor=\"#E9322E\"></td>\n");
            }

            report.AppendFormat("<td id=\"{0}\">{1}</td>\n", test.HtmlID, test.Description);
            report.Append("<td>" + FormatSourceCode(test.Source) + "</td>\n");

            string testOutput = test.ActualError.Or(FormatIntArray(test.ActualOutput));
            report.Append("<td>" +  Monospace(testOutput) + "</td>\n");

            string expectedOutput;
            if (test.ExpectError)
            {
                expectedOutput = Monospace("(error)");
            }
            else
            {
                expectedOutput = Monospace(FormatIntArray(test.ExpectedOutput));
            }
            report.Append("<td>" + expectedOutput + "</td>\n");
            report.Append("<td>" + Monospace(test.Cycles.ToString()) + "</td>\n");
            report.Append("</tr>\n");
        }

        report.Append(HtmlFooter1);
        if (firstErrorID.HasValue)
        {
            report.AppendFormat("document.getElementById(\"{0}\").scrollIntoView();", firstErrorID.Value);
        }
        report.Append(HtmlFooter2);

        File.WriteAllText(ReportFile, report.ToString());
        Process.Start("explorer.exe", ReportFile);
    }

    static int ParseNumber(string n)
    {
        if (n.ToLower().StartsWith("0x"))
        {
            return Convert.ToInt32(n, 16);
        }
        else
        {
            return Convert.ToInt32(n, 10);
        }
    }

    static int[] ParseNumberList(string list)
    {
        return list
            .Split(' ')
            .Select(x => x.Trim().ToLower())
            .Where(x => x.Length > 0)
            .Select(x => ParseNumber(x))
            .ToArray();
    }

    static Process RunProcess(string program, params string[] args)
    {
        ProcessStartInfo startInfo = new ProcessStartInfo()
        {
            FileName = program,
            Arguments = string.Join(" ", args.Select(x => string.Format("\"{0}\"", x))),
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };

        Process process = Process.Start(startInfo);
        process.WaitForExit((int)(TimeoutDuration * 1000));
        return process;
    }

    
    static string FormatInt(int n)
    {
        if (n >= 512)
        {
            return string.Format("0x{0:X}", n);
        }
        else
        {
            return n.ToString();
        }
    }
    
    static string FormatIntArray(int[] data)
    {
        return string.Join(", ", data.Select(x => FormatInt(x)));
    }

    static string FormatSourceCode(string text)
    {
        return "<pre>" + text.Trim().Replace("<", "&lt;").Replace(">", "&gt;") + "</pre>";
    }

    static string Monospace(string text)
    {
        text = text.Trim();
        if (text.Length == 0)
        {
            text = "(none)";
        }

        return string.Format("<span class=\"console\">{0}</span>", text.Replace("\n", "<br>"));
    }

    private static readonly string HtmlHeader = @"<title>Test Results</title>
<style>
body {
    font-family: sans-serif;
}
table {
    border-collapse: collapse;
}
th, td {
    vertical-align: top;
    border: 0.15em solid #CCC;
    font-size: 10pt;
    padding: 0.25em;
}
pre {
    font-size: 10pt;
}
.console {
    font-family: monospace;
    font-size: 10pt;
}
.success {
    background-color: #53F253;
    font-weight: bold;
    padding: 0.3em 0.7em;
}
.problem {
    background-color: #E9322E;
    font-weight: bold;
    padding: 0.3em 0.7em;
}
.info {
    background-color: #AAAAEE;
    font-weight: normal;
    padding: 0.3em 0.7em;
}
</style>
<h1>Test Results</h1>
</tr>";

    private static readonly string HtmlMiddle = @"<table border = '1' >
<tr>
<th></th>
<th>Description</th>
<th>Source</th>
<th>Output</th>
<th>Expected Output</th>
<th>Cycles</th>
</tr>";

    private static readonly string HtmlFooter1 = @"</table>
<script>
window.onload = function() {
    ";

    private static readonly string HtmlFooter2 = @"
};
</script>";
}
