// -*- mode: C++ -*-
//
// Copyright (c) 2007, 2008, 2009, 2010, 2011, 2013, 2014, 2015, 2017 The University of Utah
// All rights reserved.
//
// This file is part of `csmith', a random generator of C programs.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//   * Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//
//   * Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "OutputMgr.h"

#include <cassert>
#include <sstream>
#include "Common.h"
#include "CGOptions.h"
#include "platform.h"
#include "Bookkeeper.h"
#include "FunctionInvocation.h"
#include "Function.h"
#include "VariableSelector.h"
#include "CGContext.h"
#include "ExtensionMgr.h"
#include "Constant.h"
#include "ArrayVariable.h"
#include "git_version.h"
#include "random.h"
#include "util.h"

const char *OutputMgr::hash_func_name = "csmith_compute_hash";

const char *OutputMgr::step_hash_func_name = "step_hash";

static const char runtime_include[] = "\
#include \"csmith.h\"\n\
";

static const char volatile_include[] = "\
/* To use wrapper functions, compile this program with -DWRAP_VOLATILES=1. */\n\
#include \"volatile_runtime.h\"\n\
";

static const char access_once_macro[] = "\
#ifndef ACCESS_ONCE\n\
#define ACCESS_ONCE(v) (*(volatile typeof(v) *)&(v))\n\
#endif\n\
";

using namespace std;

vector<string> OutputMgr::monitored_funcs_;

std::string OutputMgr::curr_func_ = "";

void
OutputMgr::set_curr_func(const std::string &fname)
{
	OutputMgr::curr_func_ = fname;
}

bool
OutputMgr::is_monitored_func(void)
{
	if (OutputMgr::monitored_funcs_.empty())
		return true;
	std::vector<string>::iterator result =
		find(monitored_funcs_.begin(), monitored_funcs_.end(), curr_func_);
	return (result != monitored_funcs_.end());
}

OutputMgr::OutputMgr()
{

}

OutputMgr::~OutputMgr()
{

}

void
OutputMgr::OutputMain(std::ostream &out)
{
	CGContext cg_context(GetFirstFunction() /* BOGUS -- not in first func. */,
						 Effect::get_empty_effect(),
						 0);

	FunctionInvocation *invoke = NULL;
	invoke = ExtensionMgr::MakeFuncInvocation(GetFirstFunction(), cg_context);
	out << endl << endl;
	output_comment_line(out, "----------------------------------------");

	ExtensionMgr::OutputInit(out);

	// output initializers for global array variables
	OutputArrayInitializers(*VariableSelector::GetGlobalVariables(), out, 1);

	if (CGOptions::blind_check_global()) {
		ExtensionMgr::OutputFirstFunInvocation(out, invoke);
		std::vector<Variable *>& vars = *VariableSelector::GetGlobalVariables();
		for (size_t i=0; i<vars.size(); i++) {
			vars[i]->output_value_dump(out, "checksum ", 1);
		}
	}
	else {
		ExtensionMgr::OutputFirstFunInvocation(out, invoke);
		
		// resetting all global dangling pointer to null per Rohit's request
		if (!CGOptions::dangling_global_ptrs()) {
			OutputPtrResets(out, GetFirstFunction()->dead_globals);
		}

		if (CGOptions::step_hash_by_stmt())
			OutputMgr::OutputHashFuncInvocation(out, 1);
		else
			HashGlobalVariables(out);
	}
	ExtensionMgr::OutputTail(out);
	out << "}" << endl;
	delete invoke;
}

void
OutputMgr::OutputHashFuncInvocation(std::ostream &out, int indent)
{
	OutputMgr::output_tab_(out, indent);
	out << OutputMgr::hash_func_name << "();" << std::endl;
}

void
OutputMgr::OutputStepHashFuncInvocation(std::ostream &out, int indent, int stmt_id)
{
	if (is_monitored_func()) {
		OutputMgr::output_tab_(out, indent);
		out << OutputMgr::step_hash_func_name << "(" << stmt_id << ");" << std::endl;
	}
}

void
OutputMgr::OutputStepHashFuncDef(std::ostream &out)
{
	out << std::endl;
	out << "void " << OutputMgr::step_hash_func_name << "(int stmt_id)" << std::endl;
	out << "{" << std::endl;

	int indent = 1;
	OutputMgr::output_tab_(out, indent);
	out << "int i = 0;" << std::endl;
	OutputMgr::OutputHashFuncInvocation(out, indent);
	OutputMgr::output_tab_(out, indent);
	out << "printf(\"before stmt(%d): ";
	out << "checksum = %X\\n\", stmt_id, crc32_context ^ 0xFFFFFFFFUL);" << std::endl;

	OutputMgr::output_tab_(out, indent);
	out << "crc32_context = 0xFFFFFFFFUL; " << std::endl;

	OutputMgr::output_tab_(out, indent);
	out << "for (i = 0; i < 256; i++) { " << std::endl;
	OutputMgr::output_tab_(out, indent+1);
	out << "crc32_tab[i] = 0;" << std::endl;
	OutputMgr::output_tab_(out, indent);
	out << "}" << std::endl;
	OutputMgr::output_tab_(out, indent);
	out << "crc32_gentab();" << endl;
	out << "}" << std::endl;
}

void
OutputMgr::OutputHashFuncDecl(std::ostream &out)
{
	out << "void " << OutputMgr::hash_func_name << "(void);";
	out << std::endl << std::endl;
}

void
OutputMgr::OutputStepHashFuncDecl(std::ostream &out)
{
	out << "void " << OutputMgr::step_hash_func_name << "(int stmt_id);";
	out << std::endl << std::endl;
}

void
OutputMgr::OutputHashFuncDef(std::ostream &out)
{
	out << "void " << OutputMgr::hash_func_name << "(void)" << std::endl;
	out << "{" << std::endl;

	size_t dimen = Variable::GetMaxArrayDimension(*VariableSelector::GetGlobalVariables());
	if (dimen) {
		vector <const Variable*> &ctrl_vars = Variable::get_new_ctrl_vars();
		OutputArrayCtrlVars(ctrl_vars, out, dimen, 1);
	}
	HashGlobalVariables(out);
	out << "}" << std::endl;
}

void
OutputMgr::OutputTail(std::ostream &out)
{
}

void
OutputMgr::OutputHeader(int argc, char *argv[], unsigned long seed)
{
	std::ostream &out = get_main_out();

	// out << platform_include << endl;
	if (CGOptions::wrap_volatiles()) {
		out << volatile_include << endl;
	}

	if (CGOptions::access_once()) {
		out << access_once_macro << endl;
	}

	if (CGOptions::step_hash_by_stmt()) {
		OutputMgr::OutputHashFuncDecl(out);
		OutputMgr::OutputStepHashFuncDecl(out);
	}
}

void
OutputMgr::output_comment_line(ostream &out, const std::string &comment)
{
	if (CGOptions::quiet() || CGOptions::concise()) {
		outputln(out);
	}
	else {
		out << "/* " << comment << " */";
		outputln(out);
	}
}

/*
 * resetting pointers to null by outputing "p = 0;"
 */
void
OutputMgr::OutputPtrResets(ostream &out, const vector<const Variable*>& ptrs)
{
	size_t i;
	for (i=0; i<ptrs.size(); i++) {
		const Variable* v = ptrs[i];
		if (v->isArray) {
			const ArrayVariable* av = (const ArrayVariable*)v;
			Constant zero(get_int_type(), "0");
			vector<const Variable *> &ctrl_vars = Variable::get_last_ctrl_vars();
			av->output_init(out, &zero, ctrl_vars, 1);
		}
		else {
			output_tab(out, 1);
			v->Output(out);
			out << " = 0;";
			outputln(out);
		}
	}
}

void
OutputMgr::output_tab_(ostream &out, int indent)
{
	while (indent--) {
		out << TAB;
	}
}

void
OutputMgr::output_tab(ostream &out, int indent)
{
	OutputMgr::output_tab_(out, indent);
}

void
OutputMgr::really_outputln(ostream &out)
{
	out << std::endl;
}

//////////////////////////////////////////////////////////////////
