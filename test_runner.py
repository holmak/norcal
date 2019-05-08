import os
import subprocess

# Configuration
TEST_DIR = 'test_files'
TESTS_FILE = '../tests.txt'
SOURCE_FILE = 'source.c'
IMAGE_FILE = 'program.nes'
INPUT_FILE = 'input.bin'
REPORT_FILE = 'results.html'
COMPILER = '../norcal/bin/Debug/norcal.exe'
SIMULATOR = '../sim6502/x64/Debug/sim6502.exe'

os.makedirs(TEST_DIR, exist_ok=True)
os.chdir(TEST_DIR)

#############################################################################
# Read in the test cases.
#############################################################################
class Test:
    def __init__(self):
        self.description = ''
        self.source = ''
        self.disasm = ''
        self.input = []
        self.expected_output = []
        self.actual_output = []

def parse_number_list(text):
    return [int(s, 0) for s in text.split()]

tests = []

with open(TESTS_FILE, 'r') as f:
    test = Test()
    for line in f:
        if line.startswith('#'):
            # This marks the end of a test.
            # (But don't add a test with no input; this can happen at the
            # beginning of the file.)
            if test.source.strip() != '':
                tests.append(test)
                test = Test()
        # Collect metadata for the upcoming test.
        if line.startswith('# '):
            test.description = line[2:]
        elif line.startswith('#in'):
            test.input = parse_number_list(line[4:])
        elif line.startswith('#out'):
            test.expected_output = parse_number_list(line[5:])
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
for test in tests:
    test.disasm = 'N/A'
    test.passed = False
    test.html_id = 'test{}'.format(next_html_id)
    next_html_id += 1
    # Compile:
    with open(SOURCE_FILE, 'w') as f:
        f.write(test.source)
    process = run_process([COMPILER, SOURCE_FILE, IMAGE_FILE])
    if process == TIMED_OUT:
        test.actual_output = '(compiler timed out)'
        continue
    elif process.returncode != 0:
        test.actual_output = 'compiler error:\n' + process.stderr.decode('utf_8')
        continue
    # Run:
    with open(INPUT_FILE, 'wb') as f:
        # Write input data as an array of uint16_t values:
        data = bytearray()
        for n in test.input:
            data.append(n & 0xFF)
            data.append((n >> 8) & 0xFF)
        f.write(data)
    process = run_process([SIMULATOR, IMAGE_FILE, INPUT_FILE])
    if process == TIMED_OUT:
        test.actual_output = '(simulator timed out)'
        continue
    elif process.returncode != 0:
        test.actual_output = 'simulator error:\n' + process.stderr.decode('utf_8')
        continue
    test.actual_output = parse_number_list(process.stdout.decode('utf_8'))
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
    border: 0.15em solid #CCC;
    font-size: 10pt;
    padding: 0.25em;
}
pre {
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
<th>Input</th>
<th>Output</th>
<th>Expected Output</th>
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

def pre(text):
    return text.replace('\r', '').strip()

def pre_data(data):
    if type(data) is str:
        return pre(data)
    elif len(data) > 0:
        return ', '.join([format_int(n) for n in data])
    else:
        return '(none)'

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
    report.write(html_middle)
    for test in tests:
        if not test.passed and first_error_id is None:
            first_error_id = test.html_id
        report.write('<tr id="{}">\n'.format(test.html_id))
        if test.passed:
            report.write('<td bgcolor="#53F253"></td>\n')
        else:
            report.write('<td bgcolor="#E9322E"></td>\n')
        report.write('<td>' + test.description + '</td>\n')
        report.write('<td><pre>' + pre(test.source) + '</pre></td>\n')
        report.write('<td><pre>' + pre(test.disasm) + '</pre></td>\n')
        report.write('<td><pre>' + pre_data(test.input) + '</pre></td>\n')
        report.write('<td><pre>' + pre_data(test.actual_output) + '</pre></td>\n')
        report.write('<td><pre>' + pre_data(test.expected_output) + '</pre></td>\n')
        report.write('</tr>\n')
    report.write(html_footer1)
    if first_error_id is not None:
        report.write('document.getElementById("{}").scrollIntoView();'.format(first_error_id))
    report.write(html_footer2)
os.startfile(REPORT_FILE)
