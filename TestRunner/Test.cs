using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Test
{
    public string Description = "";
    public string Source = "";
    public string Disasm = "";
    public int[] ExpectedOutput = new int[0];
    public int[] ActualOutput = new int[0];
    public Maybe<string> ActualError = Maybe.Nothing;
    public bool ExpectError;
    public int Cycles;
    public string HtmlID;
    public bool Passed;
}
