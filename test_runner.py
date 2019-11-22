import os
import subprocess
import time

joinpaths = os.path.join

# Configuration
TEST_DIR = 'test_files'
DEBUG_DIR = 'debug_output'
TEST_HEADER = 'test.h'
TESTS_FILE = 'tests.txt'
RUNTIME_FILE = 'runtime.c'
SOURCE_FILE = joinpaths(TEST_DIR, 'source.c')
IMAGE_FILE = joinpaths(TEST_DIR, 'program.nes')
REPORT_FILE = joinpaths(TEST_DIR, 'results.html')
DISASM_FILE = joinpaths(DEBUG_DIR, 'dis.s')
COMPILER = 'norcal/bin/Debug/norcal.exe'
SIMULATOR = 'sim6502/x64/Debug/sim6502.exe'

os.makedirs(TEST_DIR, exist_ok=True)

def read_text_file(path):
    with open(path, 'r') as f:
        return f.read()

#############################################################################
# Read in the test cases.
#############################################################################
class Test:
    def __init__(self):
        self.description = ''
        self.source = ''
        self.disasm = ''
        self.expected_output = []
        self.actual_output = []
        self.expect_error = False
        self.cycles = 0

def parse_number_list(text):
    return [int(s, 0) for s in text.split()]

tests = []

with open(TESTS_FILE, 'r') as f:
    test = Test()
    for line in f:
        if line.startswith('@'):
            # This marks the end of a test.
            # (But don't add a test with no input; this can happen at the
            # beginning of the file.)
            if test.source.strip() != '':
                tests.append(test)
                test = Test()
        # Collect metadata for the upcoming test.
        if line.startswith('@ '):
            test.description = line[2:]
        elif line.startswith('@out'):
            test.expected_output = parse_number_list(line[5:])
        elif line.startswith('@error'):
            test.expect_error = True
        else:
            test.source += line
    # Add the final test:
    if test.source.strip() != '':
        tests.append(test)
        test = Test()

#############################################################################
# Run the tests.
#############################################################################
TIMEOUT = 0.1
TIMEOUT = None
TIMED_OUT = 'TIMED_OUT'
def run_process(*args, **kwargs):
    try:
        return subprocess.run(
            *args,
            **kwargs,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=TIMEOUT)
    except subprocess.TimeoutExpired:
        return TIMED_OUT

next_html_id = 0
previously_attached = False
time_in_compiler = 0
for test in tests:
    print('.', end='', flush=True)
    test.disasm = 'N/A'
    test.passed = False
    test.html_id = 'test{}'.format(next_html_id)
    next_html_id += 1
    # Compile:
    with open(SOURCE_FILE, 'w') as f:
        f.write(test.source)
    args = [COMPILER, RUNTIME_FILE, TEST_HEADER, SOURCE_FILE, "-o", IMAGE_FILE, "--debug-output"]
    # Automatically attach the debugger if a test fails.
    # However, only do this for the first failure, to avoid a cascade of
    # debugger prompt windows.
    # Also, don't attach if the test *expects* an error to occur.
    attach = not test.expect_error and not previously_attached
    if attach:
        args.append('--attach')
    time_before = time.perf_counter()
    process = run_process(args)
    time_after = time.perf_counter()
    time_in_compiler += (time_after - time_before)
    if process == TIMED_OUT:
        test.actual_output = '(compiler timed out)'
        test.passed = False
        continue
    elif process.returncode == 1:
        test.actual_output = 'compiler error:<br>' + process.stderr.decode('utf_8')
        test.passed = test.expect_error
        if attach and not test.passed:
            previously_attached = True
        continue
    elif process.returncode > 1:
        test.actual_output = 'compiler panic:<br>' + process.stderr.decode('utf_8')
        test.passed = False
        previously_attached = True
        continue
    if os.path.isfile(DISASM_FILE):
        test.disasm = read_text_file(DISASM_FILE)
    else:
        test.disasm = 'N/A'
    # Run:
    process = run_process([SIMULATOR, IMAGE_FILE])
    if process == TIMED_OUT:
        test.actual_output = '(simulator timed out)'
        test.passed = False
        continue
    elif process.returncode != 0:
        test.actual_output = 'simulator error:<br>' + process.stderr.decode('utf_8')
        test.passed = test.expect_error
        continue
    sim_output = parse_number_list(process.stdout.decode('utf_8'))
    test.cycles = sim_output[-1]
    test.actual_output = sim_output[:-1]
    if test.expect_error:
        test.passed = False
    else:
        test.passed = (test.actual_output == test.expected_output)

#############################################################################
# Create a report summarizing the test results.
#############################################################################
html_header = '''
<title>Test Results</title>
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
</tr>
'''

html_middle = '''
<table border="1">
<tr>
<th></th>
<th>Description</th>
<th>Source</th>
<th>Disassembly</th>
<th>Output</th>
<th>Expected Output</th>
<th>Cycles</th>
</tr>
'''

html_footer1 = '''
</table>
<script>
window.onload = function() {
    '''

html_footer2 = '''
};
</script>
'''

def format_int(n):
    if n >= 512:
        return '0x{:X}'.format(n)
    else:
        return str(n)

def monospace(data):
    text = None
    if type(data) is str:
        text = data
    elif type(data) is int:
        text = str(data)
    elif len(data) > 0:
        text = ', '.join([format_int(n) for n in data])
    else:
        text = '(none)'
    return '<span class="console">{}</span>'.format(text.strip())

first_error_id = None
with open(REPORT_FILE, 'w') as report:
    report.write(html_header)
    failures = len([x for x in tests if not x.passed])
    if failures == 0:
        report.write('<p class="success">All {} tests passed!</p>\n'.format(len(tests)))
    else:
        plural = 's' if failures > 1 else ''
        report.write('<p class="problem">{} test{} failed.</p>\n'.format(
            failures, plural))
    report.write('<p class="info">{:.3f} seconds spent in compiler; {:.3f} per test.</p>\n'.format(time_in_compiler, time_in_compiler / len(tests)))
    report.write(html_middle)
    for test in tests:
        if not test.passed and first_error_id is None:
            first_error_id = test.html_id
        report.write('<tr>\n')
        if test.passed:
            report.write('<td bgcolor="#53F253"></td>\n')
        else:
            report.write('<td bgcolor="#E9322E"></td>\n')
        report.write('<td id="{}">{}</td>\n'.format(test.html_id, test.description))
        report.write('<td><pre>' + test.source.strip() + '</pre></td>\n')
        report.write('<td><details><summary>Show</summary><pre>' + test.disasm.strip() + '</pre></details></td>\n')
        report.write('<td>' + monospace(test.actual_output) + '</td>\n')
        if test.expect_error:
            expected_output = monospace('(error)')
        else:
            expected_output = monospace(test.expected_output)
        report.write('<td>' + expected_output + '</td>\n')
        report.write('<td>' + monospace(test.cycles) + '</td>\n')
        report.write('</tr>\n')
    report.write(html_footer1)
    if first_error_id is not None:
        report.write('document.getElementById("{}").scrollIntoView();'.format(first_error_id))
    report.write(html_footer2)
os.startfile(REPORT_FILE)
