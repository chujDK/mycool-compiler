/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

static int ERR_report(char* msg);
static char error_msg[0x50];
#include <set>
#include <string>
std::set <std::string> defined_class;

%}

/*
 * Define names for regular expressions here.
 */

/*
 * tokens
 */

/* operators */

DARROW		=>
ASSIGN		<-
LE			<=
SINGLE_OPERATOR	(\+|\-|\*|\/|\~|\<|\=|\(|\)|\{|\}|\;|\:|\.|\,|\@)

/* Keywords */
CLASS		[cC][lL][aA][sS][sS]
ELSE		[eE][lL][sS][eE]
FI			[fF][iI]
IF			[iI][fF]
IN			[iI][nN]
INHERITS	[iI][nN][hH][eE][rR][iI][tT][sS]
ISVOID		[iI][sS][vV][oO][iI][dD]
LET			[lL][eE][tT]
LOOP		[lL][oO][oO][pP]
POOL		[pP][oO][oO][lL]
THEN		[tT][hH][eE][nN]
WHILE		[wW][hH][iI][lL][eE]
CASE		[cC][aA][sS][eE]
ESAC		[eE][sS][aA][cC]
NEW			[nN][eE][wW]
OF			[oO][fF]
NOT			[nN][oO][tT]
TRUE		t[rR][uU][eE]
FALSE		f[aA][lL][sS][eE]

/* basic class, token_class: TYPEID */
TYPEID		(Object|IO|String|Int|SELF_TYPE|B[oO][oO][lL]|T[rR][uU][eE]|F[aA][lL][sS][eE])

/* CONST */
INT_CONST	[0-9]+

/* White Space */
WHITESPACE 	[ \t\r\v\f]+

/* comments */

COMMENTS_LINE			--.*[^\n]

/* OBJECT */
OBJECTID	[a-zA-Z][a-zA-Z0-9_]*

%s CLASS_OCCURRED

%%

 /*
  *  Nested comments
  */

"*)"			{
	return ERR_report("Unmatched *)");
}

"(*"			{
	int c;
	int last_is_star = 0;
	int last_is_bracket = 0;
	int comment_starts_left = 1;
	for ( ; ; ) 
	{
		c = yyinput();
		if (c == '\n') 
		{
			curr_lineno++;
			last_is_star = 0;
			last_is_bracket = 0;
		}
		else if (c == '*')
		{
			if (last_is_bracket) 
			{
				comment_starts_left++;		
			}
			last_is_star = 1;
			last_is_bracket = 0;
		}
		else if (c == '(')
		{
			last_is_star = 0;
			last_is_bracket = 1;
		}
		else if (c == ')')
		{
			if (last_is_star)
			{
				comment_starts_left--;
			}
			if (comment_starts_left == 0)
			{
				break;
			}
			last_is_star = 0;
			last_is_bracket = 0;
		}
		else if (c == EOF)
		{
			return ERR_report("EOF in comment");
		}
	}
}

 /*
  *  Line comments
  */

{COMMENTS_LINE}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); 	}
{ASSIGN}		{ return (ASSIGN); 	}
{LE}			{ return (LE); 		}

 /*
  * operaotrs
  */
":"				{ BEGIN(CLASS_OCCURRED); return ':'; }
{SINGLE_OPERATOR}	{ return *((char*) yytext); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}			{ BEGIN(CLASS_OCCURRED); return (CLASS); }
{NEW}			{ BEGIN(CLASS_OCCURRED); return (NEW); }
{ELSE}			{ return (ELSE); }
{FI}			{ return (FI); }
{IF}			{ return (IF); }
{IN}			{ return (IN); }
{INHERITS}		{ return (INHERITS); }
{ISVOID}		{ return (ISVOID); }
{LET}			{ return (LET); }
{LOOP}			{ return (LOOP); }
{POOL}			{ return (POOL); }
{THEN}			{ return (THEN); }
{WHILE}			{ return (WHILE); }
{CASE}			{ return (CASE); }
{ESAC}			{ return (ESAC); }
{OF}			{ return (OF); }
{NOT}			{ return (NOT); }
{TRUE}			{
	cool_yylval.boolean = true;
	return {BOOL_CONST};
}
{FALSE}			{
	cool_yylval.boolean = false;
	return {BOOL_CONST};
}

 /*
  *  TYEPID
  */
{TYPEID}		{
	BEGIN(0);
	cool_yylval.symbol = idtable.add_string(yytext);
	return (TYPEID);
}

<CLASS_OCCURRED>{OBJECTID}	{
	BEGIN(0);
	cool_yylval.symbol = idtable.add_string(yytext);
	defined_class.insert(yytext);
	return (TYPEID);
}

 /*
  *  OBJECTID
  */
{OBJECTID} 		{
	cool_yylval.symbol = idtable.add_string(yytext);
	if (defined_class.count(yytext) == 0)
	{
		return (OBJECTID);
	}
	else
	{
		return (TYPEID);
	}
}

 /*
  *  constants 
  */

{INT_CONST}		{
	cool_yylval.symbol = inttable.add_string(yytext);
	return (INT_CONST);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

"\""			{
	char c;
	int error_flag = 0;
	int string_buf_ptr = 0;
	for ( ; ; )
	{
		if (string_buf_ptr >= (MAX_STR_CONST - 1))
		{
			strcpy(error_msg, "String constant too long");
			error_flag = 1;
			break;
		}
		c = yyinput();
		if (c == '\\')
		{
			c = yyinput();
			if (c == '\n')
			{
				curr_lineno++;
				string_buf[string_buf_ptr++] = '\n';
			}
			else if (c == 'b')
			{
				string_buf[string_buf_ptr++] = '\b';
			}
			else if (c == 't')
			{
				string_buf[string_buf_ptr++] = '\t';
			}
			else if (c == 'n')
			{
				string_buf[string_buf_ptr++] = '\n';
			}
			else if (c == 'f')
			{
				string_buf[string_buf_ptr++] = '\f';
			}
			else
			{
				string_buf[string_buf_ptr++] = c;
			}
		}
		else if (c == '\"')
		{
			break;
		}
		else if (c == '\n')
		{
			curr_lineno++;
			return ERR_report("Unterminated string constant");
		}
		else if (c == '\0')
		{
			strcpy(error_msg, "String contains null character");
			error_flag = 1;
			break;
		}
		else if (c == EOF)
		{
			return ERR_report("EOF in string constant");
		}
		else
		{
			string_buf[string_buf_ptr++] = c;
		}
	}
	string_buf[string_buf_ptr++] = 0;

	if (error_flag) 
	{
		while(c != '\"')
		{
			c = yyinput();
			if (c == '\n')
			{
				curr_lineno++;
			}
		}
		return ERR_report(error_msg);
	}
	else
	{
		cool_yylval.symbol = stringtable.add_string(string_buf);
		return (STR_CONST);
	}
}

 /*
  * process the lines
  */

[\n]			{ curr_lineno++; }
{WHITESPACE}

 /*
  * error lex
  */
([^" \\\:\n\t\f\r\v\+\-\*\/\~\<\=\(\)\{\}\;\!\@\#\$\%\^\&\'\>\_\.\[\]\,]+|\!|\@|\#|\$|\%|\^|\&|\'|\>|\_|\.|\[|\]|\,|\:|\\)	{
	return ERR_report(yytext);
}

%%

int ERR_report(char* msg)
{
	cool_yylval.error_msg = msg;
	return (ERROR);
}
