﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

class Program
{
    private static readonly string TestDir = "test_files";
    private static readonly string DebugDir = "debug_output";
    private static readonly string TestHeader = "test.h";
    private static readonly string TestsFile = "tests.txt";
    private static readonly string RuntimeFile = "runtime.c";
    private static readonly string SourceFile = Path.Combine(TestDir, "source.c");
    private static readonly string ImageFile = Path.Combine(TestDir, "program.nes");
    private static readonly string ReportFile = Path.Combine(TestDir, "results.html");
    private static readonly string DisasmFile = Path.Combine(DebugDir, "dis.s");
    private static readonly string CompilerPath = "norcal/bin/Debug/norcal.exe";
    private static readonly string SimulatorPath = "sim6502/x64/Debug/sim6502.exe";
    private static readonly float TimeoutDuration = 0.5f;
    private static readonly string TimeoutSentinel = "TIMED_OUT";

    static void Main(string[] args)
    {
        // Ensure the working directory exists:
        Directory.CreateDirectory(TestDir);

        // Load the tests from the tests file:
        List<Test> tests = new List<Test>();
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
                test.ExpectedOutput = ParseNumberList(line.Substring(5));
            }
            else if (lineLower.StartsWith("@error"))
            {
                test.ExpectError = true;
            }
            else
            {
                test.Source += line;
            }
        }
                
        // Finish the last test:
        if (test.Source.Trim() != "")
        {
            tests.Add(test);
        }

        // Run the tests:
    }

    static int[] ParseNumberList(string list)
    {
        return list
            .Split(' ')
            .Select(x => x.Trim().ToLower())
            .Where(x => x.Length > 0)
            .Select(x => int.Parse(x))
            .ToArray();
    }

    static string RunProcess(string program, params string[] args)
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
        DateTime startTime = DateTime.Now;
        while (!process.HasExited)
        {
            if (DateTime.Now - startTime > TimeSpan.FromSeconds(TimeoutDuration))
            {
                return TimeoutSentinel;
            }

            // TODO: Make sure that this isn't what is making the tests slow to run (or some equivalent code in Python).
            Thread.Sleep(10);
        }

        string stderr = process.StandardError.ReadToEnd();
        string stdout = process.StandardOutput.ReadToEnd();
        return stderr + stdout;
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
<th>Disassembly</th>
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

/*

#############################################################################
# Run the tests.
#############################################################################

next_html_id = 0
previously_attached = False
time_in_compiler = 0
for test in tests:
    print(".", end="", flush=True)
    test.disasm = "N/A"
    test.passed = False
    test.html_id = "test{}".format(next_html_id)
    next_html_id += 1
    # Compile:
    with open(SOURCE_FILE, "w") as f:
        f.write(test.source)
    args = [COMPILER, RUNTIME_FILE, TEST_HEADER, SOURCE_FILE, "-o", IMAGE_FILE]
    # Automatically attach the debugger if a test fails.
    # However, only do this for the first failure, to avoid a cascade of
    # debugger prompt windows.
    # Also, don"t attach if the test *expects* an error to occur.
    attach = not test.expect_error and not previously_attached
    if attach:
        args.append("--attach")
    time_before = time.perf_counter()
    process = run_process(args)
    time_after = time.perf_counter()
    time_in_compiler += (time_after - time_before)
    if process == TIMED_OUT:
        test.actual_output = "(compiler timed out)"
        test.passed = False
        continue
    elif process.returncode == 1:
        test.actual_output = "compiler error:<br>" + process.stderr.decode("utf_8")
        test.passed = test.expect_error
        if attach and not test.passed:
            previously_attached = True
        continue
    elif process.returncode > 1:
        test.actual_output = "compiler panic:<br>" + process.stderr.decode("utf_8")
        test.passed = False
        previously_attached = True
        continue
    if os.path.isfile(DISASM_FILE):
        test.disasm = read_text_file(DISASM_FILE)
    else:
        test.disasm = "N/A"
    # Run:
    process = run_process([SIMULATOR, IMAGE_FILE])
    if process == TIMED_OUT:
        test.actual_output = "(simulator timed out)"
        test.passed = False
        continue
    elif process.returncode != 0:
        test.actual_output = "simulator error:<br>" + process.stderr.decode("utf_8")
        test.passed = test.expect_error
        continue
    sim_output = parse_number_list(process.stdout.decode("utf_8"))
    test.cycles = sim_output[-1]
    test.actual_output = sim_output[:-1]
    if test.expect_error:
        test.passed = False
    else:
        test.passed = (test.actual_output == test.expected_output)

#############################################################################
# Create a report summarizing the test results.
#############################################################################

def format_int(n):
    if n >= 512:
        return "0x{:X}".format(n)
    else:
        return str(n)

def format_source_code(text):
    return "<pre>" + text.strip().replace("<", "&lt;").replace(">", "&gt;") + "</pre>"

def monospace(data):
    text = None
    if type(data) is str:
        text = data
    elif type(data) is int:
        text = str(data)
    elif len(data) > 0:
        text = ", ".join([format_int(n) for n in data])
    else:
        text = "(none)"
    return "<span class="console">{}</span>".format(text.strip().replace("\n", "<br>"))

first_error_id = None
with open(REPORT_FILE, "w") as report:
    report.write(html_header)
    failures = len([x for x in tests if not x.passed])
    if failures == 0:
        report.write("<p class="success">All {} tests passed!</p>\n".format(len(tests)))
    else:
        plural = "s" if failures > 1 else ""
        report.write("<p class="problem">{} test{} failed.</p>\n".format(
            failures, plural))
    report.write("<p class="info">{:.3f} seconds spent in compiler; {:.3f} per test.</p>\n".format(time_in_compiler, time_in_compiler / len(tests)))
    report.write(html_middle)
    for test in tests:
        if not test.passed and first_error_id is None:
            first_error_id = test.html_id
        report.write("<tr>\n")
        if test.passed:
            report.write("<td bgcolor="#53F253"></td>\n")
        else:
            report.write("<td bgcolor="#E9322E"></td>\n")
        report.write("<td id="{}">{}</td>\n".format(test.html_id, test.description))
        report.write("<td>" + format_source_code(test.source) + "</td>\n")
        report.write("<td><details><summary>Show</summary><pre>" + test.disasm.strip() + "</pre></details></td>\n")
        report.write("<td>" + monospace(test.actual_output) + "</td>\n")
        if test.expect_error:
            expected_output = monospace("(error)")
        else:
            expected_output = monospace(test.expected_output)
        report.write("<td>" + expected_output + "</td>\n")
        report.write("<td>" + monospace(test.cycles) + "</td>\n")
        report.write("</tr>\n")
    report.write(html_footer1)
    if first_error_id is not None:
        report.write("document.getElementById("{}").scrollIntoView();".format(first_error_id))
    report.write(html_footer2)
os.startfile(REPORT_FILE)

*/