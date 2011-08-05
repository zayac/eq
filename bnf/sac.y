%{


/*
 * $Id: sac.y 17501 2011-07-20 16:51:48Z rbe $
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "types.h"
#include "tree_basic.h"
#include "tree_compound.h"
#include "str.h"
#include "memory.h"
#define DBUG_PREFIX "PARSE"
#include "debug.h"
#include "DupTree.h"        /* for use of DUPdoDupTree() */
#include "ctinfo.h"
#include "free.h"
#include "globals.h"
#include "handle_mops.h"
#include "new_types.h"
#include "user_types.h"
#include "type_utils.h"
#include "shape.h"
#include "stringset.h"
#include "namespaces.h"
#include "check_mem.h"
#include "hidestructs.h"

#include "resource.h"


extern int commlevel;
extern int charpos;
extern char *linebuf_ptr;
extern char *yytext;

extern int yylex();


static node *global_wlcomp_aps = NULL;
static node *store_pragma = NULL;
static 
  enum {PRAG_fundef, PRAG_fundec, PRAG_objdef, PRAG_typedef} 
  pragma_type = PRAG_fundef;
static bool has_dot_rets = FALSE;
static bool has_dot_args = FALSE;

/*
 * used to distinguish the different kinds of files
 * which are parsed with this single parser
 */
static file_type file_kind = FT_prog;
static bool main_found = FALSE;
static int main_found_line = 0;
 
static int yyerror( char *errname);
static int yyparse();

static void CleanUpParser();
static node *MakeIncDecLet( char *name, char *op);
static node *MakeOpOnLet( char *name, node *name2, char *op);
static ntype *Exprs2NType( ntype *basetype, node *exprs);
static node *ConstructMop( node *, node *, node *);
static node *CheckWlcompConf( node *ap, node *exprs);
static node *SetClassType( node *module, ntype *type, node *pragmas);

static int prf_arity[] = {
  #define PRFarity( arity) arity
  #include "prf_info.mac"
};

%}

%union {
 nodetype           nodetype;
 char               *id;
 ntype              *ntype;
 node               *node;
 char               cchar;
 char               cbyte;
 short              cshort;
 int                cint;
 long               clong;
 long long          clonglong;
 unsigned char      cubyte;
 unsigned short     cushort;
 unsigned int       cuint;
 unsigned long      culong;
 unsigned long long culonglong;
 float              cfloat;
 double             cdbl;
 prf                prf;
 shape              *shape;
 resource_list_t    *resource_list_t;
 target_list_t      *target_list_t;
 inheritence_list_t *inheritence_list_t;
}

%token PARSE_PRG  PARSE_RC

%token BRACE_L  BRACE_R  BRACKET_L  BRACKET_R  SQBR_L  SQBR_R  COLON DCOLON SEMIC 
COMMA  AMPERS  DOT TWODOTS THREEDOTS QUESTION  RIGHTARROW LEFTARROW 
INLINE THREAD  LET  STRUCT  TYPEDEF  OBJDEF  CLASSTYPE 
INC  DEC  ADDON  SUBON  MULON  DIVON  MODON 
K_MAIN  RETURN  IF  ELSE  DO  WHILE  FOR  NWITH  FOLD FOLDFIX
MODULE  IMPORT  EXPORT  PROVIDE  USE  CLASS  ALL  EXCEPT DEPRECATED
SC  TRUETOKEN  FALSETOKEN  EXTERN  C_KEYWORD  GENERIC
HASH  PRAGMA  LINKNAME CUDALINKNAME  LINKSIGN  EFFECT MUTCTHREADFUN  REFCOUNTING
REFCOUNTDOTS NOINLINE
COPYFUN  FREEFUN  INITFUN  LINKWITH LINKOBJ
LOCAL WLCOMP  CACHESIM  SPECIALIZE 
TARGET  STEP  WIDTH  GENARRAY  MODARRAY  PROPAGATE
LE  LT  GT LAZYAND LAZYOR
STAR  PLUS  MINUS  TILDE  EXCL SPAWN RSPAWN
TRIANGLEBR_L TRIANGLEBR_R

PRF_DIM_A  PRF_SHAPE_A  PRF_RESHAPE_VxA  PRF_SEL_VxA  PRF_MODARRAY_AxVxS
PRF_SEL_VxIA
PRF_HIDEVALUE_SxA PRF_HIDESHAPE_SxA PRF_HIDEDIM_SxA
PRF_ADD_SxS  PRF_ADD_SxV  PRF_ADD_VxS  PRF_ADD_VxV 
PRF_SUB_SxS  PRF_SUB_SxV  PRF_SUB_VxS  PRF_SUB_VxV 
PRF_MUL_SxS  PRF_MUL_SxV  PRF_MUL_VxS  PRF_MUL_VxV 
PRF_DIV_SxS  PRF_DIV_SxV  PRF_DIV_VxS  PRF_DIV_VxV 
PRF_MOD_SxS  PRF_MOD_SxV  PRF_MOD_VxS  PRF_MOD_VxV
PRF_MIN_SxS  PRF_MIN_SxV  PRF_MIN_VxS  PRF_MIN_VxV
PRF_MAX_SxS  PRF_MAX_SxV  PRF_MAX_VxS  PRF_MAX_VxV
PRF_ABS_S  PRF_ABS_V
PRF_NEG_S  PRF_NEG_V
PRF_RECIPROC_S  PRF_RECIPROC_V
PRF_EQ_SxS   PRF_EQ_SxV  PRF_EQ_VxS  PRF_EQ_VxV
PRF_NEQ_SxS  PRF_NEQ_SxV  PRF_NEQ_VxS  PRF_NEQ_VxV
PRF_LE_SxS   PRF_LE_SxV  PRF_LE_VxS  PRF_LE_VxV
PRF_LT_SxS   PRF_LT_SxV  PRF_LT_VxS  PRF_LT_VxV
PRF_GE_SxS   PRF_GE_SxV  PRF_GE_VxS  PRF_GE_VxV
PRF_GT_SxS   PRF_GT_SxV  PRF_GT_VxS  PRF_GT_VxV
PRF_AND_SxS  PRF_AND_SxV  PRF_AND_VxS  PRF_AND_VxV
PRF_OR_SxS   PRF_OR_SxV  PRF_OR_VxS  PRF_OR_VxV
PRF_NOT_S  PRF_NOT_V
PRF_TOF_S  PRF_TOD_S PRF_TOC_S PRF_TOBOOL_S 
PRF_TOB_S  PRF_TOS_S PRF_TOI_S PRF_TOL_S PRF_TOLL_S
PRF_TOUB_S PRF_TOUS_S PRF_TOUI_S PRF_TOUL_S PRF_TOULL_S
PRF_CAT_VxV  PRF_TAKE_SxV  PRF_DROP_SxV
PRF_MESH_VxVxV
PRF_MASK_VxVxV
PRF_NON_NEG_VAL_S PRF_NON_NEG_VAL_V
PRF_VAL_LE_VAL_SxS PRF_VAL_LE_VAL_VxV

%token <id> ID  STR

%token TYPE_FLOAT  TYPE_BOOL  TYPE_UNS
       TYPE_CHAR  TYPE_DBL  TYPE_VOID  
       TYPE_BYTE TYPE_SHORT TYPE_INT TYPE_LONG TYPE_LONGLONG
       TYPE_UBYTE TYPE_USHORT TYPE_UINT TYPE_ULONG TYPE_ULONGLONG
%token <cbyte> NUMBYTE
%token <cshort> NUMSHORT
%token <cint> NUM
%token <cint> NUMINT
%token <clong> NUMLONG
%token <clonglong> NUMLONGLONG
%token <cubyte> NUMUBYTE
%token <cushort> NUMUSHORT
%token <cuint> NUMUINT
%token <culong> NUMULONG
%token <culonglong> NUMULONGLONG
%token <cfloat> FLOAT
%token <cdbl> DOUBLE
%token <cchar> CHAR

%token BURGER_L BURGER_R TFBUILTIN TFUSER TFABSTRACT
TFTYPEREL SUBTYPE IFF
%token <id> TFADD TFSUB TFMUL TFDIV
%token <id> TFLT TFGT TFLE TFGE

/*******************************************************************************
 * SAC programs
 */

%type <node> prg  defs  def1 def2  def3  def4  def5 def6

%type <node> structdef structdef2 typedef exttypedef

%type <node> objdef extobjdef

%type <node> fundef  fundef1  fundef2  fundef3  main
%type <node> fundec fundec2
%type <node> mainargs  fundecargs fundefargs  args  arg varargs
%type <node> exprblock  exprblock2  assignsOPTret  assigns  assign 
     let cond optelse  doloop whileloop forloop  assignblock
     lets qual_ext_id qual_ext_ids ids
%type <node> exprs expr subexpr nostrexpr expr_with expr_ap opt_arguments
     expr_ar expr_sel with generator  steps  width  nwithops nwithop  withop
     wlassignblock  genidx part  parts npart nparts nums returntypes
     returndectypes ntypes varntypes
%type <prf> genop prf

%type <id> reservedid  string ext_id opt_place

%type <ntype> simplentype userntype polyntype basentype ntype

/* pragmas */
%type <id> pragmacachesim
%type <node> wlcomp_pragma_global  wlcomp_pragma_local
%type <node> pragmas pragma pragmalist classpragmas
/*
%type <node> pragmas 
*/
%type <node> tfabsdef tfuserdef tfbuiltindef tfarg
%type <node> tfdef tfrel tfspec tfdefs tfrels tfexprs
%type <node> tfoperand


/*******************************************************************************
* module implementations
*/
%type <node> module class 
%type <ntype> classtype
%type <id> deprecated
%type <node> import use export provide interface
%type <node> symbolset symbolsetentries



/*******************************************************************************
* module specializations (for the C interface only!)
*/
/* %type <node> modspec */




/*******************************************************************************
* sac2crc files
*/
%type <target_list_t> targets
%type <inheritence_list_t> inherits
%type <resource_list_t> resources

%right TFLT TFGT TFLE TFGE
%left TFSUB TFADD
%left TFMUL TFDIV

%nonassoc STRUCTSET
%left STRUCTELEM
%nonassoc NOSTRUCTELEM
%left DOT

%right QUESTION COLON
%right LAZYOR
%right LAZYAND
%right INC DEC STAR PLUS MINUS TILDE EXCL LE LT GT ID
GENARRAY MODARRAY ALL AMPERS
%right BM_OP
%right MM_OP CAST
%right SQBR_L BRACKET_L TRIANGLEBR_L
%right ELSE 

%start all

%{

/*
* Make sure, the stack of the generated parser is big enough!
*/
#define YYMAXDEPTH 10000 

%}
%%

all: file eof { CleanUpParser(); }
;

file: PARSE_PRG  prg       { global.syntax_tree = $2; }
    | PARSE_PRG  module    { global.syntax_tree = $2; }
    | PARSE_PRG  class     { global.syntax_tree = $2; }
    | PARSE_RC   targets   { global.target_list = RSCaddTargetList( $2, global.target_list); }
/*  | PARSE_SPEC modspec   { spec_tree = $2; } */
;

eof: { if (commlevel) {
 CTIabortLine( global.linenum, "Unterminated comment found");

#ifdef MUST_REFERENCE_YYLABELS
/*
 * The follwing command is a veeeeeeery ugly trick to avoid warnings
 * on the alpha: the YYBACKUP-macro contains jumps to two labels
 * yyerrlab  and  yynewstate  which are not used otherwise.
 * Hence, the usage here, which in fact never IS (and never SHOULD)
 * be carried out, prevents the gcc from complaining about the two
 * aforementioned labels not to be used!!
 */
 YYBACKUP( NUM, yylval);
#endif
}
}
;


/*******************************************************************************
*******************************************************************************
*
*  rules for ordinary SAC programs
*
*******************************************************************************
*******************************************************************************/


prg: defs
     { $$ = $1;
       MODULE_NAMESPACE( $$) = NSgetRootNamespace();
       MODULE_FILETYPE( $$) = FT_prog;
     }
   ;

defs: interface def1
      { $$ = $2;
        MODULE_INTERFACE( $$) = $1;
      }
    | def1
      { $$ = $1; }

def1: tfspec def2
      { $$ = $2;
	MODULE_TYPEFAMILIES($$) = $1;
      }
    | def2
      { $$ = $1;}
    ;

def2: structdef def2
       { $$ = $2;
         STRUCTDEF_NEXT( $1) = MODULE_STRUCTS( $$);
         MODULE_STRUCTS( $$) = $1;
       }
     | def3
       { $$ = $1; }
     ;

def3: typedef def3
      { $$ = $2;
        TYPEDEF_NEXT( $1) = MODULE_TYPES( $$);
        MODULE_TYPES( $$) = $1;
      }
    | exttypedef def3
      { $$ = $2;
        TYPEDEF_NEXT( $1) = MODULE_TYPES( $$);
        MODULE_TYPES( $$) = $1;
      } 
    | exttypedef { pragma_type = PRAG_typedef; } pragmas def3
      { $$ = $4;
        TYPEDEF_NEXT( $1) = MODULE_TYPES( $$);
        TYPEDEF_PRAGMA( $1) = $3;
        MODULE_TYPES( $$) = $1;
      }
    | def4
      { $$ = $1; }
    ;

def4: objdef def4
      { $$ = $2;
        OBJDEF_NEXT( $1) = MODULE_OBJS( $$);
        MODULE_OBJS( $$) = $1;
      }
    | extobjdef def4
      { $$ = $2;
        OBJDEF_NEXT( $1) = MODULE_OBJS( $$);
        MODULE_OBJS( $$) = $1;
      }
    | extobjdef { pragma_type = PRAG_objdef; } pragmas def4
      { $$ = $4;
        OBJDEF_NEXT( $1) = MODULE_OBJS( $$);
        OBJDEF_PRAGMA( $1) = $3;
        MODULE_OBJS( $$) = $1;
      }
    | def5
      { $$ = $1; }
    ;

def5: EXTERN fundec {  pragma_type = PRAG_fundec; } pragmas def5
      { $$ = $5;
        FUNDEF_PRAGMA( $2) = $4;
        MODULE_FUNDECS( $$) = TCappendFundef( MODULE_FUNDECS( $$), $2);
      }
    | EXTERN fundec def5
      { $$ = $3;
        MODULE_FUNDECS( $$) = TCappendFundef( MODULE_FUNDECS( $$), $2);
      }
    | SPECIALIZE fundec {  pragma_type = PRAG_fundec; } pragmas def5
      { $$ = $4;
        FUNDEF_PRAGMA( $2) = $4;
        MODULE_FUNSPECS( $$) = TCappendFundef( MODULE_FUNSPECS( $$), $2);
      }
    | SPECIALIZE fundec def5
      { $$ = $3;
        MODULE_FUNSPECS( $$) = TCappendFundef( MODULE_FUNSPECS( $$), $2);
      }
    | fundef { pragma_type = PRAG_fundef; } pragmas def5
      { $$ = $4;
        MODULE_FUNS( $$) = TCappendFundef( MODULE_FUNS( $$), $1);
      }
    | fundef def5
      { $$ = $2;
        MODULE_FUNS( $$) = TCappendFundef( MODULE_FUNS( $$), $1);
      }
    | main def5
      { $$ = $2;
        MODULE_FUNS( $$) = TCappendFundef( MODULE_FUNS( $$), $1);
      } 
    | def6
      { $$ = $1; }
    ;

def6: { $$ = TBmakeModule( NULL, FT_prog, NULL, NULL, NULL, NULL, NULL);

        DBUG_PRINT("%s:"F_PTR, global.mdb_nodetype[ NODE_TYPE( $$)], $$);
      }
    ;

/*
*********************************************************************
*
*  rules for imports
*
*********************************************************************
*/
interface: import interface
           { $$ = $1;
             IMPORT_NEXT($$) = $2;
           }
         | import
           {
             $$ = $1;
           }
         | use interface
           { $$ = $1;
             USE_NEXT($$) = $2;
           }
         | use
           { $$ = $1;
           }
         | export interface
           { $$ = $1;
             EXPORT_NEXT($$) = $2;
           }
         | export
           { $$ = $1;
           }
         | provide interface
           { $$ = $1;
             PROVIDE_NEXT($$) = $2;
           }
         | provide
           { $$ = $1;
           }
         ;

import: IMPORT ID COLON ALL SEMIC
        { $$ = TBmakeImport( $2, NULL, NULL);
          IMPORT_ALL( $$) = TRUE; 
        }
        | IMPORT ID COLON ALL EXCEPT symbolset SEMIC
          { $$ = TBmakeImport( $2, NULL, $6);
            IMPORT_ALL( $$) = TRUE; 
          }
        | IMPORT ID COLON symbolset SEMIC 
          { $$ = TBmakeImport( $2, NULL, $4); }
        ;

use: USE ID COLON ALL SEMIC 
     { $$ = TBmakeUse( $2, NULL, NULL);
       USE_ALL( $$) = TRUE;
     }
   | USE ID COLON ALL EXCEPT symbolset SEMIC
     { $$ = TBmakeUse( $2, NULL, $6);
       USE_ALL( $$) = TRUE;
     }
   | USE ID COLON symbolset SEMIC { $$ = TBmakeUse( $2, NULL, $4); }
   ;

export: EXPORT ALL SEMIC 
        { $$ = TBmakeExport( NULL, NULL); 
          EXPORT_ALL( $$) = TRUE;
        }
      | EXPORT ALL EXCEPT symbolset SEMIC 
        { $$ = TBmakeExport( NULL, $4); 
          EXPORT_ALL( $$) = TRUE;
        }
      | EXPORT symbolset SEMIC { $$ = TBmakeExport( NULL, $2); }
      ;

provide: PROVIDE ALL SEMIC 
         { $$ = TBmakeProvide( NULL, NULL); 
           PROVIDE_ALL( $$) = TRUE;
         }
       | PROVIDE ALL EXCEPT symbolset SEMIC 
         { $$ = TBmakeProvide( NULL, $4); 
           PROVIDE_ALL( $$) = TRUE;
         }
       | PROVIDE symbolset SEMIC { $$ = TBmakeProvide( NULL, $2); }
       ;

symbolset: BRACE_L symbolsetentries BRACE_R { $$ = $2; } ;

symbolsetentries: ext_id COMMA symbolsetentries { $$ = TBmakeSymbol( $1, $3); }
                | ext_id { $$ = TBmakeSymbol( $1, NULL); }
                ;


/*
*********************************************************************
*
*  rules for structs
*
*********************************************************************
*/

structdef: STRUCT ID BRACE_L structdef2 /* BRACE_R in structdef2 */ SEMIC
           { $$ = TBmakeStructdef( $2, $4, NULL); }
         ;

structdef2: ntype ids SEMIC structdef2
            { node *ids_ptr = $2;
              $$ = $4;

              DBUG_ASSERT( $2 != NULL,
                           "There must be at least one ids to assign to.");

              do { /* Multiple vardecs. */
                /* I copied this one from exprblock2, but couldn't you use
                 * DUPavis for the first arg of TBmakeVardec()? Or, heck, maybe
                 * even DUPvardec for this entire loop? */
                $$ = TBmakeStructelem( STRcpy( SPIDS_NAME( $2)),
                                       TYcopyType( $1), $$);

                /* The first (of the remaining) ids has been turned into a
                 * vardec, it can now be freed. */
                ids_ptr = SPIDS_NEXT( $2);
                $2 = FREEdoFreeNode( $2);
                $2 = ids_ptr;
              } while ($2 != NULL);
            }
          | BRACE_R
            { $$ = NULL;
            }
          ;

/*
*********************************************************************
*
*  rules for typedefs
*
*********************************************************************
*/

typedef: TYPEDEF ntype ID SEMIC 
         { $$ = TBmakeTypedef( $3, NULL, $2, NULL);

           DBUG_PRINT( "%s:"F_PTR","F_PTR", Id: %s",
                       global.mdb_nodetype[ NODE_TYPE( $$)],
                       $$, 
                       TYPEDEF_NTYPE( $$),
                       TYPEDEF_NAME( $$));
         }
       ;

exttypedef: EXTERN TYPEDEF ID SEMIC
            { $$ = TBmakeTypedef( $3, NULL, TYmakeAKS(
                     TYmakeHiddenSimpleType( UT_NOT_DEFINED), 
                     SHmakeShape( 0)), NULL);

              DBUG_PRINT( "%s:"F_PTR","F_PTR", Id: %s",
                          global.mdb_nodetype[ NODE_TYPE( $$)],
                          $$, 
                          TYPEDEF_NTYPE( $$),
                          TYPEDEF_NAME( $$));
            }
          ;

/*
*********************************************************************
*
*  rules for objdefs
*
*********************************************************************
*/

objdef: OBJDEF ntype ID LET expr_ap SEMIC 
        { $$ = TBmakeObjdef( $2, NULL, $3, $5, NULL);

          DBUG_PRINT("%s:"F_PTR","F_PTR", Id: %s",
                       global.mdb_nodetype[ NODE_TYPE( $$)],
                       $$, 
                       OBJDEF_TYPE( $$),
                       OBJDEF_NAME( $$));
        }
      ;

extobjdef: EXTERN OBJDEF ntype ID SEMIC 
        {
          $$ = TBmakeObjdef( $3, NULL, $4, NULL, NULL);

          OBJDEF_ISEXTERN( $$) = TRUE;

          DBUG_PRINT( "%s:"F_PTR","F_PTR", Id: %s",
                      global.mdb_nodetype[ NODE_TYPE( $$)],
                      $$, 
                      OBJDEF_TYPE( $$),
                      OBJDEF_NAME( $$));
        }
      ;


/*
*********************************************************************
*
*  rules for fundefs
*
*********************************************************************
*/

fundef: INLINE fundef1
        { $$ = $2;
          FUNDEF_ISINLINE( $$) = TRUE;
        }
      | fundef1
        { $$ = $1;
          FUNDEF_ISINLINE( $$) = FALSE;
        }

fundef1: THREAD fundef2
         { $$ = $2;
           if (global.backend != BE_mutc) {
             yyerror( "function qualifier \"thread\" not allowed with selected backend");
           }
           FUNDEF_ISTHREADFUN( $$) = TRUE;
         }
       | fundef2
         { $$ = $1;
           FUNDEF_ISTHREADFUN( $$) = FALSE;
         }
       ;

fundef2: returntypes BRACKET_L ext_id BRACKET_R BRACKET_L fundef3
         { $$ = $6;
           FUNDEF_RETS( $$) = $1; /* result type(s) */
           FUNDEF_NAME( $$) = $3; /* function name  */
           FUNDEF_ALLOWSINFIX( $$) = TRUE;

           DBUG_PRINT(  "%s: %s:%s "F_PTR,
                        global.mdb_nodetype[ NODE_TYPE( $$)],
                        (FUNDEF_NS( $$) == NULL) 
                          ? "(null)"
                          : NSgetName( FUNDEF_NS( $$)),
                        FUNDEF_NAME( $$),
                        FUNDEF_NAME( $$));

         }
       | returntypes ext_id BRACKET_L fundef3
         { $$ = $4;
           FUNDEF_RETS( $$) = $1;   /* result type(s) */
           FUNDEF_NAME( $$) = $2;    /* function name  */
           FUNDEF_ALLOWSINFIX( $$) = FALSE;

           DBUG_PRINT(  "%s: %s:%s "F_PTR,
                        global.mdb_nodetype[ NODE_TYPE( $$)],
                        (FUNDEF_NS( $$) == NULL) 
                          ? "(null)" 
                          : NSgetName( FUNDEF_NS( $$)),
                        FUNDEF_NAME( $$),
                        FUNDEF_NAME( $$));

         }
       ;

fundef3: fundefargs BRACKET_R
         { $<node>$ = TBmakeFundef( NULL, NULL, NULL, NULL, NULL, NULL); }
         exprblock
         { 
           $<node>$ = $<node>3;
           FUNDEF_BODY( $$) = $4;             /* function body  */
           FUNDEF_ARGS( $$) = $1;             /* fundef args */

           DBUG_PRINT(  "%s:"F_PTR", Id: %s"F_PTR" %s," F_PTR,
                        global.mdb_nodetype[ NODE_TYPE( $$)],
                        $$, 
                        global.mdb_nodetype[ NODE_TYPE( FUNDEF_BODY( $$))],
                        FUNDEF_BODY( $$),
                        global.mdb_nodetype[ NODE_TYPE( FUNDEF_ARGS( $$))],
                        FUNDEF_ARGS( $$));
         }
       | BRACKET_R 
         { $<node>$ = TBmakeFundef( NULL, NULL, NULL, NULL, NULL, NULL); }
         exprblock
         { $<node>$ = $<node>2;
           FUNDEF_BODY( $$) = $3;

           DBUG_PRINT(  "%s:"F_PTR" %s"F_PTR,
                        global.mdb_nodetype[ NODE_TYPE( $$)],
                        $$, 
                        global.mdb_nodetype[ NODE_TYPE( FUNDEF_BODY( $$))],
                        FUNDEF_BODY( $$));
         }
       ;


fundefargs: args        { $$ = $1;   }
          | TYPE_VOID   { $$ = NULL; }
          ;

fundecargs: varargs     { $$ = $1;   }
          | TYPE_VOID   { $$ = NULL; }
          | THREEDOTS   { $$ = NULL; has_dot_args = TRUE; }
          ;


args: arg COMMA args
      { ARG_NEXT($1) = $3;
        $$ = $1;
      }
    | arg
     { $$ = $1; 
      }
    ;

varargs: arg COMMA varargs
         { ARG_NEXT($1) = $3;
           $$ = $1;
         }
       | arg
        { $$ = $1;
         }
       | arg COMMA THREEDOTS
         { $$ = $1;
           has_dot_args = TRUE;
         }
       ;


arg: ntype ID
     { $$ = TBmakeArg( TBmakeAvis( $2, $1), NULL);

       AVIS_DECLTYPE( ARG_AVIS( $$)) = TYcopyType( $1);

       DBUG_PRINT(  "%s: "F_PTR", Id: %s ",
                    global.mdb_nodetype[ NODE_TYPE( $$)],
                    $$, 
                    ARG_NAME( $$));
     }
   | ntype AMPERS ID
     { $$ = TBmakeArg( TBmakeAvis( $3, $1), NULL);
       ARG_ISREFERENCE( $$) = TRUE;

       AVIS_DECLTYPE( ARG_AVIS( $$)) = TYcopyType( $1);

       DBUG_PRINT(  "%s: "F_PTR", Id: %s, Attrib: %d ",
                    global.mdb_nodetype[ NODE_TYPE( $$)],
                    $$, 
                    ARG_NAME( $$));
     }
   ;


main: TYPE_INT K_MAIN BRACKET_L mainargs BRACKET_R { $<cint>$ = global.linenum; } exprblock
      { $$ = TBmakeFundef( NULL, NULL,
                           TBmakeRet(
                             TYmakeAKS( TYmakeSimpleType( T_int), SHmakeShape(0)),
                             NULL),
                           $4, $7, NULL);
        NODE_LINE( $$) = $<cint>6;

        if (file_kind != FT_prog) {
          CTIerrorLine( global.linenum, 
                        "Defintion of main function found in module/class implementation");
        }
        
        if (main_found) {
          CTIerrorLine( global.linenum,
                        "Repeated definition of main function, "
                        "previous definition in line %d", main_found_line);
        }
        
        FUNDEF_NAME( $$) = STRcpy( "main");
        FUNDEF_ISMAIN( $$) = TRUE;

        main_found = TRUE;
        main_found_line = global.linenum;

        DBUG_PRINT(  "%s:"F_PTR", main "F_PTR " %s (" F_PTR ") ",
                     global.mdb_nodetype[ NODE_TYPE( $$)],
                     $$, 
                     FUNDEF_NAME( $$),
                     global.mdb_nodetype[ NODE_TYPE( FUNDEF_BODY($$))],
                     FUNDEF_BODY($$));
      }
    ;

mainargs: TYPE_VOID     { $$ = NULL; }
        | /* empty */   { $$ = NULL; }
        ;


/*
*********************************************************************
*
*  rules for fundecs
*
*********************************************************************
*/

fundec: returndectypes ext_id BRACKET_L fundec2
        { $$ = $4;
          FUNDEF_RETS( $$) = $1;
          FUNDEF_NAME( $$) = $2;  /* function name */
          FUNDEF_ISEXTERN( $$) = TRUE;
          if( has_dot_rets) {
             FUNDEF_HASDOTRETS( $$) = TRUE;
             has_dot_rets = FALSE;
           }
        }
      ;

fundec2: fundecargs BRACKET_R { $<cint>$ = global.linenum; } SEMIC
         { $$ = TBmakeFundef( NULL, NULL, NULL, $1, NULL, NULL);
           NODE_LINE( $$) = $<cint>3;
           if( has_dot_args) {
             FUNDEF_HASDOTARGS( $$) = TRUE;
             has_dot_args = FALSE;
           }
         }
       | BRACKET_R { $<cint>$ = global.linenum; } SEMIC
         { $$ = TBmakeFundef( NULL, NULL, NULL, NULL, NULL, NULL);
           NODE_LINE( $$) = $<cint>2;
         }
       ;

/*
*********************************************************************
*
*  rules for pragmas
*
*********************************************************************
*/

hash_pragma: HASH PRAGMA ;

wlcomp_pragma_global: hash_pragma WLCOMP expr_ap
                      { if (global_wlcomp_aps != NULL) {
                          /* remove old global pragma */
                          global_wlcomp_aps = FREEdoFreeTree( global_wlcomp_aps);
                        }
                        $3 = CheckWlcompConf( $3, NULL);
                        if ($3 != NULL) {
                          global_wlcomp_aps = $3;
                        }
                      }
                    ;

wlcomp_pragma_local: hash_pragma WLCOMP expr_ap
                     { $3 = CheckWlcompConf( $3, NULL);
                       if ($3 != NULL) {
                         $$ = TBmakePragma();
                         PRAGMA_WLCOMP_APS( $$) = $3;
                       } else {
                         $$ = NULL;
                       }
                     }
                   | /* empty */
                     { if (global_wlcomp_aps != NULL) {
                         $$ = TBmakePragma();
                         PRAGMA_WLCOMP_APS( $$) = DUPdoDupTree( global_wlcomp_aps);
                       } else {
                         $$ = NULL;
                       }
                     }
                   ;


pragmacachesim: hash_pragma CACHESIM string   { $$ = $3;              }
              | hash_pragma CACHESIM          { $$ = STRcpy( ""); }
              | /* empty */              { $$ = NULL;            }
              ;


/*
 * pragmas as needed for external functions
 *  
 */
pragmas: pragmalist
         { $$ = store_pragma;
           store_pragma = NULL;
         }
       ;

pragmalist: pragma pragmalist
          | pragma
          | wlcomp_pragma_global
          ;

pragma: hash_pragma LINKNAME string
        { if( (pragma_type != PRAG_fundec) && (pragma_type != PRAG_objdef) ) {
            yyerror( "pragma \"linkname\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_LINKNAME( store_pragma) != NULL) {
            CTIwarnLine( global.linenum, 
                         "Conflicting definitions of pragma 'linkname`");
          }
          PRAGMA_LINKNAME( store_pragma) = $3;
        }
      | hash_pragma CUDALINKNAME string
        { if( pragma_type != PRAG_fundec) {
            yyerror( "pragma \"cudalinkname\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_CUDALINKNAME( store_pragma) != NULL) {
            CTIwarnLine( global.linenum, 
                         "Conflicting definitions of pragma 'cudalinkname`");
          }
          PRAGMA_CUDALINKNAME( store_pragma) = $3;
        }
      | hash_pragma LINKWITH string
        { if( (pragma_type != PRAG_fundec) && (pragma_type != PRAG_typedef) ) {
            yyerror( "pragma \"linkwith\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_LINKMOD( store_pragma) = STRSadd( $3, STRS_extlib,
                                            PRAGMA_LINKMOD( store_pragma));
        }
      | hash_pragma LINKOBJ string
        { if( (pragma_type != PRAG_fundec) && (pragma_type != PRAG_typedef) ) {
            yyerror( "pragma \"linkobj\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_LINKOBJ( store_pragma) = STRSadd( $3, STRS_objfile,
                                            PRAGMA_LINKOBJ( store_pragma));
        }
      | hash_pragma LINKSIGN SQBR_L nums SQBR_R
        { if( pragma_type != PRAG_fundec) {
            yyerror( "pragma \"linksign\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_LINKSIGN( store_pragma) != NULL) {
            CTIwarnLine( global.linenum,
                         "Conflicting definitions of pragma 'linksign`");
          }
          PRAGMA_LINKSIGN( store_pragma) = $4;
        }
      | hash_pragma REFCOUNTING SQBR_L nums SQBR_R
        { if( pragma_type != PRAG_fundec) {
            yyerror( "pragma \"refcounting\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_REFCOUNTING( store_pragma) != NULL) {
            CTIwarnLine( global.linenum, 
                         "Conflicting definitions of pragma 'refcounting`");
          }
          PRAGMA_REFCOUNTING( store_pragma) = $4;
        }
      | hash_pragma REFCOUNTDOTS
        { if( pragma_type != PRAG_fundec) {
            yyerror( "pragma \"refcountdots\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_REFCOUNTDOTS( store_pragma) = TRUE;
        }
      | hash_pragma EFFECT qual_ext_ids
        { if( pragma_type != PRAG_fundec)  {
            yyerror( "pragma \"effect\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_EFFECT( store_pragma) = $3;
        }
      | hash_pragma MUTCTHREADFUN
        { if (global.backend != BE_mutc) {
            yyerror( "pragma \"mutcthreadfun\" not allowed with selected backend");
          }
          if (pragma_type != PRAG_fundec && pragma_type != PRAG_fundef) {
            yyerror( "pragma \"mutcthreadfun\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_MUTCTHREADFUN( store_pragma) = TRUE;
        }
      | hash_pragma NOINLINE
        { 
          if (pragma_type != PRAG_fundec && pragma_type != PRAG_fundef) {
            yyerror( "pragma \"noinline\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          PRAGMA_NOINLINE( store_pragma) = TRUE;
        }
      | hash_pragma COPYFUN string
        { if( pragma_type != PRAG_typedef) {
            yyerror( "pragma \"copyfun\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_COPYFUN( store_pragma) != NULL) {
            CTIwarnLine( global.linenum, 
                         "Conflicting definitions of pragma 'copyfun`");
          }
          PRAGMA_COPYFUN( store_pragma) = $3;
        }
      | hash_pragma FREEFUN string
        { if( pragma_type != PRAG_typedef) {
            yyerror( "pragma \"freefun\" not allowed here");
          }
          if (store_pragma == NULL) {
            store_pragma = TBmakePragma();
          }
          if (PRAGMA_FREEFUN( store_pragma) != NULL) {
            CTIwarnLine( global.linenum,
                         "Conflicting definitions of pragma 'freefun`");
          }
          PRAGMA_FREEFUN( store_pragma) = $3;
        }
      ;


/*
 *********************************************************************
 *
 *  rules for expression blocks
 *
 *********************************************************************
 */

exprblock: BRACE_L { $<cint>$ = global.linenum; } pragmacachesim exprblock2
           { $$ = $4;
             BLOCK_CACHESIM( $$) = $3;
             NODE_LINE( $$) = $<cint>2;
           }
         ;

exprblock2: ntype ids SEMIC exprblock2
            { node *vardec_ptr;
              node *ids_ptr = $2;

              /*
               * Insert the actual vardec(s) before the ones that
               * are already attached to the N_block node of $4!
               * This reverses the order of var-decs!
               * The reason for doing so is feasablilty only...
               * In regard to semantics, there should be no difference...
               */
              vardec_ptr = BLOCK_VARDEC( $4);

              DBUG_ASSERT( $2 != NULL,
                           "non-terminal ids should not return NULL ptr!");

              /*
               * In the AST, each variable has its own declaration.
               * Therefore, for each ID in ids, we have to generate
               * its own N_vardec node with its own copy of the
               * types-structure from $1!
               */
              while (SPIDS_NEXT( $2) != NULL) {  /* at least 2 vardecs! */
                vardec_ptr = TBmakeVardec( 
                               TBmakeAvis( STRcpy( SPIDS_NAME( $2)),
                                           TYcopyType( $1)),
                               vardec_ptr);

                /*
                 * set the DECL_TYPE as well!
                 */
                AVIS_DECLTYPE( VARDEC_AVIS( vardec_ptr)) = TYcopyType( $1);

                /*
                 * Now, we want to "push" $2 one IDS further
                 * and we want to FREE the current SPIDS node.
                 */
                ids_ptr = SPIDS_NEXT( $2);
                $2 = FREEdoFreeNode( $2);
                $2 = ids_ptr;
              }
              /*
               * When we reach this point, all but one vardec is constructed!
               * Therefore, we can recycle the ntype from $1 instead of
               * duplicating it as done in the loop above!
               */
              $$ = $4;
              BLOCK_VARDEC( $$) = TBmakeVardec( 
                                    TBmakeAvis( 
                                      STRcpy( SPIDS_NAME( $2)), $1),
                                                vardec_ptr);
              /*
               * set the DECL_TYPE for the last vardec!
               */
              AVIS_DECLTYPE( VARDEC_AVIS( BLOCK_VARDEC( $$))) 
                = TYcopyType( $1);

              /* 
               * Finally, we free the last SPIDS-node! 
               */
              $2 = FREEdoFreeTree( $2);   
            }
          | assignsOPTret BRACE_R
            { $$ = TBmakeBlock( $1, NULL);
            }
          ;

assignsOPTret: /*
                * Although this rule is very similar to the "assigns"
                * rule we keep both of them since this allows the
                * N_assign -> N_return     node to be
                * inserted directly in the N_assign chain.
                * Otherwise, we would have to append that node
                * to a given N_assigns chain as generated by
                * "assigns".
                */
               /* empty */
               { $$ = TBmakeAssign( TBmakeReturn( NULL), NULL);
               }
             | RETURN BRACKET_L { $<cint>$ = global.linenum; } exprs BRACKET_R SEMIC
               { $$ = TBmakeAssign( TBmakeReturn( $4), NULL);
                 NODE_LINE( $$) = $<cint>3;
               }
             | RETURN BRACKET_L { $<cint>$ = global.linenum; } BRACKET_R SEMIC
               { $$ = TBmakeAssign( TBmakeReturn( NULL), NULL);
                 NODE_LINE( $$) = $<cint>3;
               }
             | RETURN { $<cint>$ = global.linenum; } SEMIC
               { $$ = TBmakeAssign( TBmakeReturn( NULL), NULL);
                 NODE_LINE( $$) = $<cint>2;
               }
             | assign { $<cint>$ = global.linenum; } assignsOPTret
               {
                 $$ = TCappendAssign( $1, $3);
                 NODE_LINE($$) = $<cint>2;
                 /*
                  * $1 may be a former for-loop in which case it may point
                  * to an entire chain of assignments.
                  */
                     }
             ;

assigns: /* empty */
         { $$ = NULL;
         }
       | assign { $<cint>$ = global.linenum; } assigns
         { 
           $$ = TCappendAssign( $1, $3);
           NODE_LINE($$) = $<cint>2;
           /*
            * $1 may be a former for-loop in which case it may point
            * to an entire chain of assignments.
            */
         }
       ;

assign: let SEMIC       { $$ = TBmakeAssign( $1, NULL); }
      | cond            { $$ = TBmakeAssign( $1, NULL); }
      | doloop SEMIC    { $$ = TBmakeAssign( $1, NULL); }
      | whileloop       { $$ = TBmakeAssign( $1, NULL); }
      | forloop         { $$ = $1; /* forloop already produces assign node. */}
      ;

let:       ids LET { $<cint>$ = global.linenum; } expr
           { $$ = TBmakeLet( $1, $4);
             NODE_LINE( $$) = $<cint>3;
           }
         /* Struct element assignment. */
         | ID DOT ID LET { $<cint>$ = global.linenum; } expr
           {
             node *funcall;
             node *exprs;

             exprs = TBmakeExprs( $6, TBmakeExprs( TBmakeSpid( NULL, STRcpy($1)), NULL));
             funcall = TBmakeSpap( TBmakeSpid( NULL, STRcat( STRUCT_SET, $3)), exprs);
             $$ = TBmakeLet( TBmakeSpids( STRcpy( $1), NULL), funcall);
             NODE_LINE( $$) = $<cint>5;
           }
         | ID SQBR_L exprs SQBR_R LET { $<cint>$ = global.linenum; } expr
           { node *id, *ids, *ap;

             if( TCcountExprs( $3) > 1) {
               $3 = TCmakeVector( TYmakeAKS( TYmakeSimpleType( T_unknown), 
                                             SHmakeShape(0)),
                                  $3);
             } else {
               node * tmp;

               tmp = $3;
               $3 = EXPRS_EXPR( $3);
               EXPRS_EXPR( tmp) = NULL;
               tmp = FREEdoFreeNode( tmp);
             }
             id = TBmakeSpid( NULL, STRcpy( $1));

             ids = TBmakeSpids( $1, NULL);

             ap = TBmakeSpap( 
                            TBmakeSpid( NULL, STRcpy( "modarray")),
                            TBmakeExprs( id,
                              TBmakeExprs( $3,
                                TBmakeExprs( $7,
                                  NULL))));

             $$ = TBmakeLet( ids, ap);
             NODE_LINE( $$) = $<cint>6;
           }
         | expr_ap { $$ = TBmakeLet( NULL, $1); }
         | expr_with { $$ = TBmakeLet( NULL, $1); }
         | ID INC { $$ = MakeIncDecLet( $1, STRcpy( "+")); }
         | INC ID { $$ = MakeIncDecLet( $2, STRcpy( "+")); }
         | ID DEC { $$ = MakeIncDecLet( $1, STRcpy( "-")); }
         | DEC ID { $$ = MakeIncDecLet( $2, STRcpy( "-")); }
         | ID ADDON expr { $$ = MakeOpOnLet( $1, $3, STRcpy("+")); }
         | ID SUBON expr { $$ = MakeOpOnLet( $1, $3, STRcpy("-")); }
         | ID MULON expr { $$ = MakeOpOnLet( $1, $3, STRcpy("*")); }
         | ID DIVON expr { $$ = MakeOpOnLet( $1, $3, STRcpy("/")); }
         | ID MODON expr { $$ = MakeOpOnLet( $1, $3, STRcpy("%")); }
         ;

cond: IF { $<cint>$ = global.linenum; } BRACKET_L expr BRACKET_R assignblock optelse
      {
        $$ = TBmakeCond( $4, $6, $7);
        NODE_LINE( $$) = $<cint>2;
      }
      ;

optelse: ELSE assignblock           { $$ = $2;                 }
       | /* empty */   %prec ELSE   { $$ = MAKE_EMPTY_BLOCK(); }
       ;

doloop: DO { $<cint>$ = global.linenum; } assignblock
        WHILE BRACKET_L expr BRACKET_R 
        {
          $$ = TBmakeDo( $6, $3);
          NODE_LINE( $$) = $<cint>2;
        }
      ;

whileloop: WHILE { $<cint>$ = global.linenum; } BRACKET_L expr BRACKET_R
           assignblock
           {
             $$ = TBmakeWhile( $4, $6);
             NODE_LINE( $$) = $<cint>2;
           }
         ;

forloop:   FOR { $<cint>$ = global.linenum; }
           BRACKET_L lets SEMIC expr SEMIC lets BRACKET_R assignblock
           { /*
              *    for (e1; e2; e3) {assigns}
              *    
              *    is transformed into
              *    
              *     e1;
              *     while (e2) {
              *       assigns
              *       e3;
              *     }
              *
              *    Note that e1 and e3 are potentially empty comma-separated lists of 
              *    assignments while e2 is an expression which must evaluate to a 
              *    boolean value.
              */

             node *while_assign;
             
             BLOCK_INSTR( $10) = TCappendAssign( BLOCK_INSTR( $10), $8);
             while_assign = TBmakeAssign( TBmakeWhile( $6, $10), NULL);
             NODE_LINE( while_assign) = $<cint>2;
             NODE_LINE( ASSIGN_INSTR( while_assign)) = $<cint>2;
             $$ = TCappendAssign( $4, while_assign);
           }
         ;


lets: let COMMA lets
      {
        $$ = TBmakeAssign( $1, $3);
      }
      | let
      {
        $$ = TBmakeAssign( $1, NULL);
      }
      |
      {
        $$ = NULL;
      }
    ; 

assignblock: SEMIC
             { $$ = MAKE_EMPTY_BLOCK();
             }
           | BRACE_L { $<cint>$ = global.linenum; } pragmacachesim assigns BRACE_R
             { if ($4 == NULL) {
                 $$ = MAKE_EMPTY_BLOCK();
               }
               else {
                 $$ = TBmakeBlock( $4, NULL);
                 BLOCK_CACHESIM( $$) = $3;
                 NODE_LINE( $$) = $<cint>2;
               }
             }
           | assign
             { $$ = TBmakeBlock( $1, NULL);
             }
           ;


/*
 *********************************************************************
 *
 *  rules for expressions
 *
 *********************************************************************
 */

exprs: expr COMMA exprs          { $$ = TBmakeExprs( $1, $3);   }
     | expr                      { $$ = TBmakeExprs( $1, NULL); }
     ;


expr: DOT                        { $$ = TBmakeDot( 1);        }
    | THREEDOTS                  { $$ = TBmakeDot( 3);        }
    | subexpr %prec STRUCTELEM   { $$ = $1;                   }
    ;


/* This used to call trouble on Solaris.
 */
subexpr: subexpr DOT ID %prec STRUCTELEM
               {
                 $$ = TBmakeSpap( TBmakeSpid( NULL, STRcat( STRUCT_GET, $3)),
                         TBmakeExprs( $$, NULL));
               }
       | nostrexpr %prec NOSTRUCTELEM { $$ = $1; }
       ;


/* Expression without struct-style member access (x.y). */
nostrexpr:
      qual_ext_id                { $$ = $1; }
    | NUMBYTE                    { $$ = TBmakeNumbyte( $1);   }
    | NUMSHORT                   { $$ = TBmakeNumshort( $1);  }
    | NUMINT                     { $$ = TBmakeNumint( $1);    }
    | NUMLONG                    { $$ = TBmakeNumlong( $1);   }
    | NUMLONGLONG                { $$ = TBmakeNumlonglong( $1); }
    | NUMUBYTE                   { $$ = TBmakeNumubyte( $1);  }
    | NUMUSHORT                  { $$ = TBmakeNumushort( $1); }
    | NUMUINT                    { $$ = TBmakeNumuint( $1);   }
    | NUMULONG                   { $$ = TBmakeNumulong( $1);  }
    | NUMULONGLONG               { $$ = TBmakeNumulonglong( $1); }
    | NUM                        { $$ = TBmakeNum( $1);       }
    | FLOAT                      { $$ = TBmakeFloat( $1);     }
    | DOUBLE                     { $$ = TBmakeDouble( $1);    }
    | CHAR                       { $$ = TBmakeChar( $1);      }
    | string                     { $$ = STRstring2Array( $1); MEMfree( $1);  }
    | TRUETOKEN                  { $$ = TBmakeBool( 1);       }
    | FALSETOKEN                 { $$ = TBmakeBool( 0);       }
    | expr LAZYAND expr          { $$ = TBmakeFuncond( $1, $3, TBmakeBool(0)); }
    | expr LAZYOR expr           { $$ = TBmakeFuncond( $1, TBmakeBool(1), $3); }
    | expr QUESTION expr COLON expr 
      { $$ = TBmakeFuncond( $1, $3, $5); 
      }
    |  BRACKET_L expr BRACKET_R
      { $$ = $2;
        if( NODE_TYPE( $2) == N_spmop) {
          SPMOP_ISFIXED( $$) = TRUE;
        }
      }
    | expr qual_ext_id expr %prec BM_OP
      {
        $$ = ConstructMop( $1, $2, $3);
      }
    | PLUS expr %prec MM_OP
      {
        $$ = TCmakeSpap1( NULL, STRcpy( "+"), $2);
      }
    | MINUS expr %prec MM_OP
      {
        $$ = TCmakeSpap1( NULL, STRcpy( "-"), $2);
      }
    | TILDE expr %prec MM_OP
      {
        $$ = TCmakeSpap1( NULL, STRcpy( "~"), $2);
      }
    | EXCL expr %prec MM_OP
      {
        $$ = TCmakeSpap1( NULL, STRcpy( "!"), $2);
      }
    | PLUS BRACKET_L expr COMMA exprs BRACKET_R
      {
        $$ = TBmakeSpap( TBmakeSpid( NULL, STRcpy( "+")), 
                         TBmakeExprs( $3, $5));
      }
    | MINUS BRACKET_L expr COMMA exprs BRACKET_R
      {
        $$ = TBmakeSpap( TBmakeSpid( NULL, STRcpy( "-")), 
                         TBmakeExprs( $3, $5));
      }
    | TILDE BRACKET_L expr COMMA exprs BRACKET_R
      {
        $$ = TBmakeSpap( TBmakeSpid( NULL, STRcpy( "~")), 
                         TBmakeExprs( $3, $5));
      }
    | EXCL BRACKET_L expr COMMA exprs BRACKET_R
      {
        $$ = TBmakeSpap( TBmakeSpid( NULL, STRcpy( "!")), 
                         TBmakeExprs( $3, $5));
      }
    | expr_sel                    { $$ = $1; }   /* bracket notation      */
    | expr_ap                     { $$ = $1; }   /* prefix function calls */
    | expr_with                   { $$ = $1; }   /* with loops            */
    | expr_ar                     { $$ = $1; }   /* constant arrays       */
    | BRACKET_L COLON ntype BRACKET_R expr   %prec CAST
      { $$ = TBmakeCast( $3, $5);
      }
    | BRACE_L ID RIGHTARROW expr BRACE_R
      { $$ = TBmakeSetwl( TBmakeSpid( NULL, STRcpy( $2)), $4);
      }
    | BRACE_L SQBR_L exprs SQBR_R RIGHTARROW expr BRACE_R
      { $$ = TBmakeSetwl( $3, $6);
      }
    ;
      
expr_with: NWITH { $<cint>$ = global.linenum; } with
           { $$ = $3;
             NODE_LINE( $$)= $<cint>2;
           } 
         | LOCAL NWITH { $<cint>$ = global.linenum; } with /* Used only with mutc */
           { $$ = $4;
             NODE_LINE( $$)= $<cint>3;
             WITH_DIST( $$) = STRcpy( "PLACE_LOCAL");
           }     
         ;

with: BRACKET_L generator BRACKET_R wlassignblock withop
      { 
        node *cexpr, *code;
#if 1
        /*
         * For now, we do not yet ask for the new syntax, BUT later we will
         * activate the following two lines....
         */
        CTIwarnLine( global.linenum, 
                     "Old with-loop style deprecated!\n"
                     "Please use the new syntax instead");
#endif

        /*
         * the tricky part about this rule is that $5 (an N_withop node)
         * carries the goal-expression of the With-Loop, i.e., the "N-expr"
         * node which belongs into the N_code node!!!
         * The reason for this is that an exclusion of the goal expression
         * from the non-terminal withop would lead to a shift/reduce
         * conflict in that rule!
         */
        if( NODE_TYPE( $5) == N_genarray) {
          cexpr = GENARRAY_SPEXPR($5);
          GENARRAY_SPEXPR($5) = NULL;
        } else if ( NODE_TYPE( $5) == N_modarray) {
          cexpr = MODARRAY_SPEXPR($5);
          MODARRAY_SPEXPR($5) = NULL;
        } else {
          cexpr = SPFOLD_SPEXPR($5);
        }
        code = TBmakeCode( $4, TBmakeExprs( cexpr, NULL));
        CODE_USED( code)++;

        $$ = TBmakeWith( $2, code, $5);
        /*
         * Finally, we generate the link between the (only) partition
         * and the (only) code!
         */
        PART_CODE( WITH_PART( $$)) = code;
      }
    | BRACKET_L ID BRACKET_R parts nwithop
      { $$ = $4;
        WITH_WITHOP( $$) = $5;
#if 1
        /*
         * At the time being we ignore $2. However, it SHOULD not be specified
         * here.....
         */
        CTIwarnLine( global.linenum,
                     "Extra specification of with-loop index is deprecated!\n"
                     "Please eliminate the unifying index in brackets!");
#endif

      }
    | BRACE_L wlcomp_pragma_local nparts BRACE_R COLON 
      BRACKET_L nwithops BRACKET_R
      { $$ = $3;
        WITH_WITHOP( $$) = $7;
        WITH_PRAGMA( $$) = $2;
      }
    | BRACE_L wlcomp_pragma_local BRACE_R COLON 
      BRACKET_L nwithops BRACKET_R
      { $$ = TBmakeWith( NULL, NULL, $6);
        WITH_PRAGMA( $$) = $2;
      }
    | BRACE_L wlcomp_pragma_local nparts BRACE_R COLON nwithop
      { $$ = $3;
        WITH_WITHOP( $$) = $6;
        WITH_PRAGMA( $$) = $2;
      }
    | BRACE_L wlcomp_pragma_local BRACE_R COLON nwithop
      { $$ = TBmakeWith( NULL, NULL, $5);
        WITH_PRAGMA( $$) = $2;
      }
    | BRACE_L wlcomp_pragma_local nparts BRACE_R COLON TYPE_VOID
      { $$ = $3;
        WITH_WITHOP( $$) = NULL;
        WITH_PRAGMA( $$) = $2;
      }
    | COLON BRACKET_L nwithops BRACKET_R
      {
        $$ = TBmakeWith( NULL, NULL, $3);
      }
    | COLON TYPE_VOID
      {
        $$ = TBmakeWith( NULL, NULL, NULL);
      }
    | COLON nwithop
      {
        $$ = TBmakeWith( NULL, NULL, $2);
      }
    ;


expr_sel: expr SQBR_L exprs SQBR_R
          { if( TCcountExprs($3) == 1) {
              $$ = TCmakeSpap2( NULL, STRcpy( "sel"),
                                EXPRS_EXPR( $3), $1);
              EXPRS_EXPR( $3) = NULL;
              $3 = FREEdoFreeNode( $3);
            } else {
              $$ = 
                TCmakeSpap2(NULL, STRcpy( "sel"),
                            TCmakeVector(TYmakeAKS(TYmakeSimpleType(T_unknown),
                                                   SHmakeShape(0)),
                                         $3),
                            $1);
            }
          }
        | expr SQBR_L SQBR_R
          { $$ = 
              TCmakeSpap2( NULL, STRcpy( "sel"),
                           TCmakeVector(TYmakeAKS( TYmakeSimpleType(T_unknown), 
                                                   SHmakeShape(0)),
                                        NULL), 
                           $1);
          }
        ;

expr_ap: qual_ext_id BRACKET_L { $<cint>$ = global.linenum; } opt_arguments BRACKET_R
         {
           $$ = TBmakeSpap( $1, $4);
           NODE_LINE( $$) = $<cint>3;
         }
       | prf BRACKET_L { $<cint>$ = global.linenum; } opt_arguments BRACKET_R
         { char tmp[64];
           int num_args;

           num_args = TCcountExprs( $4);
           if( num_args != prf_arity[$1]) {
             sprintf( tmp, "%d argument(s) expected instead of %d", prf_arity[$1], num_args);
             yyerror( tmp);
           } else {
             $$ = TBmakePrf( $1, $4);
           }
           NODE_LINE( $$) = $<cint>3;
         }
       | SPAWN opt_place qual_ext_id BRACKET_L { $<cint>$ = global.linenum; } opt_arguments BRACKET_R
         {
           $$ = TBmakeSpap( $3, $6);
           if (global.fp || (global.backend == BE_mutc)) {
             /* only parse spawn if support is enabled, otherwise ignore it */
             SPAP_ISSPAWNED( $$) = TRUE;
             if ( $2 != NULL){
               SPAP_SPAWNPLACE( $$) = $2;
             }
           }
           NODE_LINE( $$) = $<cint>5;
         }
       | RSPAWN opt_place qual_ext_id BRACKET_L { $<cint>$ = global.linenum; } opt_arguments BRACKET_R
         {
           $$ = TBmakeSpap( $3, $6);
           if (global.fp || (global.backend == BE_mutc)) {
             /* only parse spawn if support is enabled, otherwise ignore it */
             SPAP_ISSPAWNED( $$) = TRUE;
             SPAP_ISREMOTE( $$) = TRUE;
             if ( $2 != NULL){
               SPAP_SPAWNPLACE( $$) = $2;
             }
           }
           NODE_LINE( $$) = $<cint>5;
         }
       ;

opt_place: BRACKET_L STR BRACKET_R { $$ = $2;   }
           | /* empty */           { $$ = NULL; }

opt_arguments: exprs         { $$ = $1;   }
             | /* empty */   { $$ = NULL; }
             ;

expr_ar: SQBR_L { $<cint>$ = global.linenum; } exprs SQBR_R
         { $$ = TCmakeVector( TYmakeAKS( TYmakeSimpleType( T_unknown), 
                                         SHmakeShape(0)),
                              $3);
           NODE_LINE( $$) = $<cint>2;
         }
       | SQBR_L { $<cint>$ = global.linenum; } COLON ntype SQBR_R
         { if ( !TYisAKS( $4)) {
             yyerror("Empty array with non-constant shape found.");
           }
           $$ = TCmakeVector( $4, NULL);
           NODE_LINE( $$) = $<cint>2;
         }
       | SQBR_L { $<cint>$ = global.linenum; } SQBR_R
         { $$ = TCmakeVector( TYmakeAKS( TYmakeSimpleType( T_int), 
                                         SHmakeShape(0)), 
                              NULL);
           NODE_LINE( $$) = $<cint>2;
         }
       | TRIANGLEBR_L { $<cint>$ = global.linenum; } exprs TRIANGLEBR_R
         { $$ = TCmakeVector( TYmakeAKS( TYmakeSimpleType( T_unknown), 
                                         SHmakeShape(0)),
                              $3);
           ARRAY_ISIRREGULAR($$) = TRUE;
           NODE_LINE( $$) = $<cint>2;
         }
       ;

nparts: npart
       { $$ = $1;
       }
     | npart nparts
       { $$ = $1;
         PART_NEXT( WITH_PART( $1)) = WITH_PART( $2);
         CODE_NEXT( WITH_CODE( $1)) = WITH_CODE( $2);
         WITH_PART( $2) = NULL;
         WITH_CODE( $2) = NULL;
         FREEdoFreeTree( $2);
       }
     ;

npart: BRACKET_L generator BRACKET_R wlassignblock COLON expr SEMIC
       { $$ = TBmakeWith( $2, TBmakeCode( $4, TBmakeExprs( $6, NULL)), NULL);
         CODE_USED( WITH_CODE( $$))++;
         PART_CODE( $2) = WITH_CODE( $$);
       }
     | BRACKET_L generator BRACKET_R wlassignblock COLON
                                    BRACKET_L expr COMMA exprs BRACKET_R SEMIC
       { $$ = TBmakeWith( $2, TBmakeCode( $4, TBmakeExprs( $7, $9)), NULL);
         CODE_USED( WITH_CODE( $$))++;
         PART_CODE( $2) = WITH_CODE( $$);
       }
     | BRACKET_L generator BRACKET_R wlassignblock SEMIC
       { $$ = TBmakeWith( $2, TBmakeCode( $4, NULL), NULL);
         CODE_USED( WITH_CODE( $$))++;
         PART_CODE( $2) = WITH_CODE( $$);
       }
     ;
     
parts: part
       { $$ = $1;
       }
     | part parts
       { $$ = $1;
         PART_NEXT( WITH_PART( $1)) = WITH_PART( $2);
         CODE_NEXT( WITH_CODE( $1)) = WITH_CODE( $2);
         WITH_PART( $2) = NULL;
         WITH_CODE( $2) = NULL;
         FREEdoFreeTree( $2);
       }
     ;

part: BRACKET_L generator BRACKET_R wlassignblock COLON exprs SEMIC
      { $$ = TBmakeWith( $2, TBmakeCode( $4, $6), NULL);
        CODE_USED( WITH_CODE( $$))++;
        PART_CODE( $2) = WITH_CODE( $$);
      }
    | BRACKET_L generator BRACKET_R wlassignblock SEMIC
      { $$ = TBmakeWith( $2, TBmakeCode( $4, NULL), NULL);
        CODE_USED( WITH_CODE( $$))++;
        PART_CODE( $2) = WITH_CODE( $$);
      }
    ;
     
generator: expr LE genidx genop expr steps width
           {
             if( ($7 != NULL) && ($6 == NULL)) {
               CTIwarnLine( global.linenum,
                            "Width vector ignored due to missing step vector");
               $7 = FREEdoFreeTree( $7);
             }
             $$ = TBmakePart( NULL,
                              $3,
                              TBmakeGenerator( F_wl_le, $4, $1, $5, $6, $7));
           }
         | expr LT genidx genop expr steps width
           {
             if( ($7 != NULL) && ($6 == NULL)) {
               CTIwarnLine( global.linenum,
                            "Width vector ignored due to missing step vector");
               $7 = FREEdoFreeTree( $7);
             }
             $$ = TBmakePart( NULL,
                              $3,
                              TBmakeGenerator( F_wl_lt, $4, $1, $5, $6, $7));
           }
         ;

steps: /* empty */   { $$ = NULL; }
     | STEP expr     { $$ = $2;   }
     ;

width: /* empty */   { $$ = NULL; }
     | WIDTH expr    { $$ = $2;   }
     ;

genidx: ID LET SQBR_L ids SQBR_R
        { $$ = TBmakeWithid( TBmakeSpids( STRcpy( $1), NULL), $4);
        }
      | ID
        { $$ = TBmakeWithid( TBmakeSpids( STRcpy( $1), NULL), NULL);
        }
      | SQBR_L ids SQBR_R
        { $$ = TBmakeWithid( NULL, $2);
        }
      ;


genop: LT   { $$ = F_wl_lt; }
     | LE   { $$ = F_wl_le; }
     ;


wlassignblock: BRACE_L { $<cint>$ = global.linenum; } assigns BRACE_R
               { if ($3 == NULL) {
                   $$ = MAKE_EMPTY_BLOCK();
                 }
                 else {
                   $$ = TBmakeBlock( $3, NULL);
                 }
                 NODE_LINE( $$) = $<cint>2;
               }
             | /* empty */
               { $$ = MAKE_EMPTY_BLOCK();
               }
             ;

nwithops: nwithop
          { $$ = $1;
          }
        | nwithop COMMA nwithops
          { $$ = $1;
            L_WITHOP_NEXT( $$, $3);
          }
        ;

nwithop: GENARRAY BRACKET_L expr COMMA expr BRACKET_R
         { $$ = TBmakeGenarray( $3, $5);
         }
       | GENARRAY BRACKET_L expr BRACKET_R
         { $$ = TBmakeGenarray( $3, NULL);
         }
       | MODARRAY BRACKET_L expr BRACKET_R
         { $$ = TBmakeModarray( $3);
         }
       | FOLD BRACKET_L qual_ext_id COMMA expr BRACKET_R
         { $$ = TBmakeSpfold( $5);
           SPFOLD_FUN( $$) = STRcpy( SPID_NAME( $3));
           SPFOLD_NS( $$) = NSdupNamespace( SPID_NS( $3));
           $3 = FREEdoFreeTree( $3);
         }
       | FOLDFIX BRACKET_L qual_ext_id COMMA expr COMMA expr BRACKET_R
         { $$ = TBmakeSpfold( $5);
           SPFOLD_FUN( $$) = STRcpy( SPID_NAME( $3));
           SPFOLD_NS( $$) = NSdupNamespace( SPID_NS( $3));
           SPFOLD_GUARD( $$) = $7;
           $3 = FREEdoFreeTree( $3);
         }
       | PROPAGATE BRACKET_L expr BRACKET_R
         { $$ = TBmakePropagate( $3);
         }
       ;

withop: GENARRAY BRACKET_L expr COMMA expr BRACKET_R
        { $$ = TBmakeGenarray( $3, NULL);
          GENARRAY_SPEXPR( $$) = $5;
        }
      | GENARRAY BRACKET_L expr COMMA expr COMMA expr BRACKET_R
        { $$ = TBmakeGenarray( $3, $7);
          GENARRAY_SPEXPR( $$) = $5;
        }
      | MODARRAY BRACKET_L expr COMMA ID COMMA expr BRACKET_R
        { $$ = TBmakeModarray( $3);
          MODARRAY_SPEXPR( $$) = $7;
        }
      | FOLD BRACKET_L qual_ext_id COMMA expr COMMA expr BRACKET_R
        { $$ = TBmakeSpfold( $5);
          SPFOLD_FUN( $$) = STRcpy( SPID_NAME( $3));
          SPFOLD_NS( $$) = NSdupNamespace( SPID_NS( $3));
          $3 = FREEdoFreeTree( $3);
          SPFOLD_SPEXPR( $$) = $7;
        }
      | FOLDFIX BRACKET_L qual_ext_id COMMA expr COMMA expr COMMA expr BRACKET_R
        { $$ = TBmakeSpfold( $5);
          SPFOLD_FUN( $$) = STRcpy( SPID_NAME( $3));
          SPFOLD_NS( $$) = NSdupNamespace( SPID_NS( $3));
          $3 = FREEdoFreeTree( $3);
          SPFOLD_GUARD( $$) = $7;
          SPFOLD_SPEXPR( $$) = $9;
        }
      ;

prf: PRF_DIM_A          { $$ = F_dim_A;     }
   | PRF_SHAPE_A        { $$ = F_shape_A;   }
   | PRF_RESHAPE_VxA    { $$ = F_reshape_VxA; }
   | PRF_SEL_VxA        { $$ = F_sel_VxA;     }
   | PRF_MODARRAY_AxVxS { $$ = F_modarray_AxVxS;}
   | PRF_SEL_VxIA       { $$ = F_sel_VxIA;  }
   | PRF_HIDEVALUE_SxA  { $$ = F_hideValue_SxA;}
   | PRF_HIDESHAPE_SxA  { $$ = F_hideShape_SxA;}
   | PRF_HIDEDIM_SxA    { $$ = F_hideDim_SxA;}
   | PRF_ADD_SxS        { $$ = F_add_SxS; }
   | PRF_ADD_SxV        { $$ = F_add_SxV; }
   | PRF_ADD_VxS        { $$ = F_add_VxS; }
   | PRF_ADD_VxV        { $$ = F_add_VxV; }
   | PRF_SUB_SxS        { $$ = F_sub_SxS; }
   | PRF_SUB_SxV        { $$ = F_sub_SxV; }
   | PRF_SUB_VxS        { $$ = F_sub_VxS; }
   | PRF_SUB_VxV        { $$ = F_sub_VxV; }
   | PRF_MUL_SxS        { $$ = F_mul_SxS; }
   | PRF_MUL_SxV        { $$ = F_mul_SxV; }
   | PRF_MUL_VxS        { $$ = F_mul_VxS; }
   | PRF_MUL_VxV        { $$ = F_mul_VxV; }
   | PRF_DIV_SxS        { $$ = F_div_SxS; }
   | PRF_DIV_SxV        { $$ = F_div_SxV; }
   | PRF_DIV_VxS        { $$ = F_div_VxS; }
   | PRF_DIV_VxV        { $$ = F_div_VxV; }
   | PRF_MOD_SxS        { $$ = F_mod_SxS; }
   | PRF_MOD_SxV        { $$ = F_mod_SxV; }
   | PRF_MOD_VxS        { $$ = F_mod_VxS; }
   | PRF_MOD_VxV        { $$ = F_mod_VxV; }
   | PRF_ABS_S          { $$ = F_abs_S;   }
   | PRF_ABS_V          { $$ = F_abs_V;   }
   | PRF_NEG_S          { $$ = F_neg_S;   }
   | PRF_NEG_V          { $$ = F_neg_V;   }
   | PRF_RECIPROC_S     { $$ = F_reciproc_S;   }
   | PRF_RECIPROC_V     { $$ = F_reciproc_V;   }
   | PRF_MIN_SxS        { $$ = F_min_SxS; }
   | PRF_MIN_SxV        { $$ = F_min_SxV; }
   | PRF_MIN_VxS        { $$ = F_min_VxS; }
   | PRF_MIN_VxV        { $$ = F_min_VxV; }
   | PRF_MAX_SxS        { $$ = F_max_SxS; }
   | PRF_MAX_SxV        { $$ = F_max_SxV; }
   | PRF_MAX_VxS        { $$ = F_max_VxS; }
   | PRF_MAX_VxV        { $$ = F_max_VxV; }
   | PRF_EQ_SxS         { $$ = F_eq_SxS;  }
   | PRF_EQ_SxV         { $$ = F_eq_SxV;  }
   | PRF_EQ_VxS         { $$ = F_eq_VxS;  }
   | PRF_EQ_VxV         { $$ = F_eq_VxV;  }
   | PRF_NEQ_SxS        { $$ = F_neq_SxS; }
   | PRF_NEQ_SxV        { $$ = F_neq_SxV; }
   | PRF_NEQ_VxS        { $$ = F_neq_VxS; }
   | PRF_NEQ_VxV        { $$ = F_neq_VxV; }
   | PRF_LT_SxS         { $$ = F_lt_SxS;  }
   | PRF_LT_SxV         { $$ = F_lt_SxV;  }
   | PRF_LT_VxS         { $$ = F_lt_VxS;  }
   | PRF_LT_VxV         { $$ = F_lt_VxV;  }
   | PRF_LE_SxS         { $$ = F_le_SxS;  }
   | PRF_LE_SxV         { $$ = F_le_SxV;  }
   | PRF_LE_VxS         { $$ = F_le_VxS;  }
   | PRF_LE_VxV         { $$ = F_le_VxV;  }
   | PRF_GT_SxS         { $$ = F_gt_SxS;  }
   | PRF_GT_SxV         { $$ = F_gt_SxV;  }
   | PRF_GT_VxS         { $$ = F_gt_VxS;  }
   | PRF_GT_VxV         { $$ = F_gt_VxV;  }
   | PRF_GE_SxS         { $$ = F_ge_SxS;  }
   | PRF_GE_SxV         { $$ = F_ge_SxV;  }
   | PRF_GE_VxS         { $$ = F_ge_VxS;  }
   | PRF_GE_VxV         { $$ = F_ge_VxV;  }
   | PRF_NOT_S          { $$ = F_not_S;   }
   | PRF_NOT_V          { $$ = F_not_V;   }
   | PRF_AND_SxS        { $$ = F_and_SxS; }
   | PRF_AND_SxV        { $$ = F_and_SxV; }
   | PRF_AND_VxS        { $$ = F_and_VxS; }
   | PRF_AND_VxV        { $$ = F_and_VxV; }
   | PRF_OR_SxS         { $$ = F_or_SxS;  }
   | PRF_OR_SxV         { $$ = F_or_SxV;  }
   | PRF_OR_VxS         { $$ = F_or_VxS;  }
   | PRF_OR_VxV         { $$ = F_or_VxV;  }
   | PRF_TOB_S          { $$ = F_tob_S;   }
   | PRF_TOS_S          { $$ = F_tos_S;   }
   | PRF_TOI_S          { $$ = F_toi_S;   }
   | PRF_TOL_S          { $$ = F_tol_S;   }
   | PRF_TOLL_S         { $$ = F_toll_S;  }
   | PRF_TOUB_S         { $$ = F_toub_S;  }
   | PRF_TOUS_S         { $$ = F_tous_S;  }
   | PRF_TOUI_S         { $$ = F_toui_S;  }
   | PRF_TOUL_S         { $$ = F_toul_S;  }
   | PRF_TOULL_S        { $$ = F_toull_S; }
   | PRF_TOF_S          { $$ = F_tof_S;   }
   | PRF_TOD_S          { $$ = F_tod_S;   }
   | PRF_TOC_S          { $$ = F_toc_S;   }
   | PRF_TOBOOL_S       { $$ = F_tobool_S;   }
   | PRF_CAT_VxV        { $$ = F_cat_VxV; }
   | PRF_TAKE_SxV       { $$ = F_take_SxV;}
   | PRF_DROP_SxV       { $$ = F_drop_SxV;}
   | PRF_MASK_VxVxV     { $$ = F_mask_VxVxV; }
   | PRF_NON_NEG_VAL_S  { $$ = F_non_neg_val_S; }
   | PRF_NON_NEG_VAL_V  { $$ = F_non_neg_val_V; }
   | PRF_VAL_LE_VAL_SxS { $$ = F_val_le_val_SxS; }
   | PRF_VAL_LE_VAL_VxV { $$ = F_val_le_val_VxV; }
   ;

qual_ext_ids: qual_ext_id COMMA qual_ext_ids
              { $$ = TBmakeExprs( $1, $3);
              }
            | qual_ext_id
              { $$ = TBmakeExprs( $1, NULL);
              }
            ;

qual_ext_id: ext_id
             { $$ = TBmakeSpid( NULL, $1);
             }
           | ID DCOLON ext_id
             { $$ = TBmakeSpid( NSgetNamespace( $1), $3);
             }
           ; 

ext_id: ID         { $$ = $1; } 
      | reservedid { $$ = $1; }
      ; 

ids: ID COMMA ids
     { $$ = TBmakeSpids( $1, $3);
     }
   | ID
     { $$ = TBmakeSpids( $1, NULL);
     }
   ;

reservedid: GENARRAY          { $$ = STRcpy("genarray"); }
          | MODARRAY          { $$ = STRcpy("modarray"); }
          | ALL               { $$ = STRcpy("all"); }
          | AMPERS            { $$ = STRcpy("&"); }
          | EXCL              { $$ = STRcpy("!"); }
          | INC               { $$ = STRcpy("++"); }
          | DEC               { $$ = STRcpy("--"); }
          | PLUS              { $$ = STRcpy("+"); }
          | MINUS             { $$ = STRcpy("-"); }
          | STAR              { $$ = STRcpy("*"); }
          | LE                { $$ = STRcpy("<="); }
          | LT                { $$ = STRcpy("<"); }
          | GT                { $$ = STRcpy(">"); }
          ; 
string: STR       
        { $$ = $1;
        }
      | STR string
        { $$ = STRcat( $1, $2);
          $1 = MEMfree( $1);
          $2 = MEMfree( $2);
        }
      ;

nums: NUM COMMA nums { $$ = TBmakeNums( $1, $3); }
      | NUM { $$ = TBmakeNums( $1, NULL); }
      ;

/*
 **********************************************************
 *
 *  rules for typefamily (tf)
 *
 ********************************************************** 
 */
tfspec: tfdefs tfrels
	{
	  $$=TBmakeTfspec($1,$2);
	}

tfdefs: tfdef tfdefs
	{
	  $$=TBmakeTfvertex($1,NULL,NULL,$2);
	}
	|tfdef
	{
	  $$=TBmakeTfvertex($1,NULL,NULL,NULL);
	}
	;
tfrels: tfrel tfrels
	{
	  TFREL_NEXT($1)=$2;
	  $$=$1;
	}
	|tfrel
	{
	  $$=$1;
	}
	;
tfdef: tfabsdef
	{
	  $$=$1;
	}
	|tfuserdef
	{
	  $$=$1;
	}
	|tfbuiltindef
	{
	  $$=$1;
	}
	;
tfabsdef: TFABSTRACT ID tfarg SEMIC
	{
	  $$=TBmakeTfabs($2,$3);
	}
	|TFABSTRACT ID SEMIC
	{
	  $$=TBmakeTfabs($2,NULL);
	}
	;
tfuserdef: TFUSER ID tfarg SEMIC
	{
	  $$=TBmakeTfusr($2,$3);
	}
	;
tfbuiltindef: 
	TFBUILTIN simplentype SEMIC
	{
	  $$=TBmakeTfbin(TYtype2DebugString($2,0,0), NULL);
	}
	| TFBUILTIN ID SEMIC
	{
	  $$=TBmakeTfbin($2, NULL);
	}
	| TFBUILTIN ID BURGER_L tfarg BURGER_R SEMIC
	{
	  $$=TBmakeTfbin($2, $4);
	}
	;
tfarg: ID DCOLON ID COMMA tfarg
	{
	  $$=TBmakeTfarg($1,$3,$5);
	}
	|
	ID COMMA tfarg
	{
	  $$=TBmakeTfarg($1,NULL,$3);
	}
	|
	ID DCOLON ID
	{
	  $$=TBmakeTfarg($1,$3,NULL);
	}
	|
	ID
	{
	  $$=TBmakeTfarg($1,NULL,NULL);
	}
	;
tfrel: TFTYPEREL simplentype SUBTYPE simplentype SEMIC
	{
	  $$=TBmakeTfrel(TYtype2DebugString($2,0,0),
		TYtype2DebugString($4,0,0),NULL,NULL);
	}
	|TFTYPEREL ID SUBTYPE ID SEMIC
	{
	  $$=TBmakeTfrel($2,$4,NULL,NULL);
	}
	|TFTYPEREL simplentype SUBTYPE ID SEMIC
	{
	  $$=TBmakeTfrel(TYtype2DebugString($2,0,0),$4,NULL,NULL);
	}
	|TFTYPEREL ID SUBTYPE ID IFF tfexprs SEMIC
	{
	  $$=TBmakeTfrel($2,$4,$6,NULL);
	}
	;
tfexprs: 
	tfexprs DOT LT tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy("<"),$1,$4);
	}
	| tfexprs TFGT tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| tfexprs DOT LE tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy("<="),$1,$4);
	}
	| tfexprs TFGE tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| tfexprs TFMUL tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| tfexprs TFDIV tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| tfexprs TFADD tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| tfexprs TFSUB tfexprs
	{
	  $$=TBmakeTfexpr(STRcpy($2),$1,$3);
	}
	| BRACKET_L tfexprs BRACKET_R
	{
	  $$=$2;
	}
	| tfoperand
	{
	  $$=$1;
	}
	;

tfoperand : ID
	{
	  $$=TBmakeTfexpr(NULL,NULL,NULL);
	  TFEXPR_ASSIGNEEID($$)=STRcpy($1);
	}
	|
	NUM
	{
	  $$=TBmakeTfexpr(NULL,NULL,NULL);
	  TFEXPR_VALUE($$)=$1;
	}
	;


/*
 *********************************************************************
 *
 *  rules for ntype
 *
 *********************************************************************
 */

returntypes: TYPE_VOID   { $$ = NULL; }
           | ntypes      { $$ = $1;   }
           ;

returndectypes: TYPE_VOID   { $$ = NULL; }
              | varntypes   { $$ = $1;   }
              | THREEDOTS   { $$ = NULL; has_dot_rets = TRUE; }
              ;

ntypes: ntype COMMA ntypes { $$ = TBmakeRet( $1, $3); }
      | ntype { $$ = TBmakeRet( $1,NULL); }
      ;

varntypes: ntype COMMA ntypes { $$ = TBmakeRet( $1, $3); }
         | ntype { $$ = TBmakeRet( $1,NULL); }
         | ntype COMMA THREEDOTS
         { $$ = TBmakeRet( $1,NULL);
           has_dot_rets = TRUE;
         }
         ;

ntype: basentype
       { $$ = TYmakeAKS( $1, SHmakeShape(0)); 
       }
     | basentype SQBR_L SQBR_R
       { $$ = TYmakeAKS( $1, SHmakeShape(0)); 
       }
     | basentype SQBR_L exprs SQBR_R
       { $$ = Exprs2NType( $1, $3);
       }
     ;

basentype: simplentype
           { $$ = $1;
           }
         | userntype
           { $$ = $1;
           }
         | polyntype
           { $$ = $1;
           }
         ;

simplentype: TYPE_BYTE       { $$ = TYmakeSimpleType( T_byte);       }
           | TYPE_SHORT      { $$ = TYmakeSimpleType( T_short);      }
           | TYPE_INT        { $$ = TYmakeSimpleType( T_int);        }
           | TYPE_LONG       { $$ = TYmakeSimpleType( T_long);       }
           | TYPE_LONGLONG   { $$ = TYmakeSimpleType( T_longlong);   }
           | TYPE_UBYTE      { $$ = TYmakeSimpleType( T_ubyte);      }
           | TYPE_USHORT     { $$ = TYmakeSimpleType( T_ushort);     }
           | TYPE_UINT       { $$ = TYmakeSimpleType( T_uint);       }
           | TYPE_ULONG      { $$ = TYmakeSimpleType( T_ulong);      }
           | TYPE_ULONGLONG  { $$ = TYmakeSimpleType( T_ulonglong);  }
           | TYPE_FLOAT      { $$ = TYmakeSimpleType( T_float);      }
           | TYPE_BOOL       { $$ = TYmakeSimpleType( T_bool);       }
           | TYPE_CHAR       { $$ = TYmakeSimpleType( T_char);       }
           | TYPE_DBL        { $$ = TYmakeSimpleType( T_double);     }
           ;

userntype: STRUCT ID
         /* This replaces all `struct <id>' occurences, when used as a ntype,
          * by `_struct_<id>'. Struct definitions are later replaced by typedefs
          * of such identifiers as hidden types, so whenever the programmer
          * thinks he is using a struct he is just using a typedef'ed hidden
          * type. The typechecker will take care of the rest.
          */
           { $$ = TYmakeSymbType( STRcat( STRUCT_TYPE, $2), NULL);
           }
         | ID
           { $$ = TYmakeSymbType( $1, NULL);
           }
         | ID DCOLON ID
           { $$ = TYmakeSymbType( $3, NSgetNamespace( $1));
           }
         ;

polyntype: LT ID LET ID SQBR_L ID SQBR_R GT
           { $$ = TYmakePolyUserType( $2, $4, $6, FALSE, FALSE); 
           }
         | LT ID RIGHTARROW ID SQBR_L ID SQBR_R GT
           { $$ = TYmakePolyUserType( $2, $4, $6, TRUE, FALSE); 
           }
         | LT ID LEFTARROW ID SQBR_L ID SQBR_R GT
           { $$ = TYmakePolyUserType( $2, $4, $6, FALSE, TRUE); 
           }
         | LT ID GT
           { $$ = TYmakePolyType( $2);
           }
         ;

 ******************************************************************************
 *
 *  rules for module implementations
 *
 *  std-rules reused:
 *
 *    - defs
 *    - type
 *
 ******************************************************************************
 ******************************************************************************/

module: MODULE { file_kind = FT_modimp; } ID deprecated SEMIC defs
        { $$ = $6;
          MODULE_NAMESPACE( $$) = NSgetNamespace( $3);
          MODULE_FILETYPE( $$) = file_kind;
          MODULE_DEPRECATED( $$) = $4;
        }
        ;

class: CLASS { file_kind = FT_classimp; } ID deprecated SEMIC classtype 
       classpragmas defs
       { $$ = $8;
         MODULE_NAMESPACE( $$) = NSgetNamespace( $3);
         MODULE_FILETYPE( $$) = file_kind;
         MODULE_DEPRECATED( $$) = $4;
         $$ = SetClassType( $$, $6, $7);
       }
       ;

classpragmas: /* empty */
              { $$ = NULL;
              }
            | { pragma_type = PRAG_typedef; } pragmas
              { $$ = $2;
              }
            ;

deprecated: /* empty */
            { $$ = NULL;
            }
          | DEPRECATED STR 
            { $$ = $2;
            }
          ;

classtype: CLASSTYPE ntype SEMIC { $$ = $2; }
         | EXTERN CLASSTYPE SEMIC 
           { 
             $$ = TYmakeAKS(
                    TYmakeHiddenSimpleType( UT_NOT_DEFINED),
                    SHmakeShape( 0)); 
           }
         ;


/*******************************************************************************
 *******************************************************************************
 *
 *  rules for sac2crc files
 *
 *  std-rules reused:
 *
 *    - string
 *
 *******************************************************************************
 *******************************************************************************/


targets: TARGET ID inherits resources targets
         { $$ = RSCmakeTargetListEntry( $2, $3, $4, $5);
         }
       | /* empty */
         { $$ = NULL;
         }
       ;

inherits: DCOLON ID inherits
           { $$ = RSCmakeInheritenceListEntry( $2, $3);
           }
        | COLON
          { $$ = NULL;
          } 
        ;

resources: ID COLON LET string resources
           { $$ = RSCmakeResourceListEntry( $1, $4, 0, 0, $5);
           }
         | ID ADDON string resources
           { $$ = RSCmakeResourceListEntry( $1, $3, 0, 1, $4);
           }
         | ID COLON LET NUM resources
           { $$ = RSCmakeResourceListEntry( $1, NULL, $4, 0, $5);
           }
         | ID ADDON NUM resources
           { $$ = RSCmakeResourceListEntry( $1, NULL, $3, 1, $4);
           }
         | /* empty */
           { $$ = NULL;
           }
         ;


%%


/*
 *********************************************************************
 *
 *  functions 
 *
 *********************************************************************
 */



/******************************************************************************
 *
 * Function:
 *   int SPmyYyparse()
 *
 * Description:
 *   
 *
 ******************************************************************************/

int SPmyYyparse()
{
  char *tmp;

  DBUG_ENTER (); 

  /* 
   * make a copy of the actual filename, which will be used for
   * all subsequent nodes...
   */
  tmp = (char *) MEMmalloc( (STRlen(global.filename)+1) * sizeof( char));
  CHKMdoNotReport( tmp);
  strcpy( tmp, global.filename);
  global.filename = tmp;

#if YYDEBUG
  DBUG_EXECUTE_TAG ("YACC", yydebug=1;);
#endif

  DBUG_RETURN (yyparse());
}



/******************************************************************************
 *
 * Function:
 *   int yyerror( char *errname)
 *
 * Description:
 *   
 *
 ******************************************************************************/

static
int yyerror( char *errname)
{
  int offset = 0;
  int size_of_output;
  
  DBUG_ENTER ();

  charpos -= (STRlen( yytext) - 1);

  size_of_output = CTIgetErrorMessageLineLength();
  
  if (STRlen( linebuf_ptr) > (size_t) size_of_output) {
    if (charpos >= size_of_output - 15) {
      offset = charpos - size_of_output + 15;
      strncpy( linebuf_ptr + offset, "... ", 4);
    }
    strcpy( linebuf_ptr + offset + size_of_output - 4, " ...");
  }

  CTIabortLine( global.linenum,
                "%s at pos %d: '%s`\n%s\n%*s",
                errname, 
                charpos, 
                yytext,
                linebuf_ptr + offset,
                charpos - offset, "^");

  DBUG_RETURN (0);
}


/******************************************************************************
 *
 * Function:
 *   void CleanUpParser()
 *
 * Description:
 *   
 *
 ******************************************************************************/

static
void CleanUpParser()
{
  DBUG_ENTER ();

  if (global_wlcomp_aps != NULL) {
    global_wlcomp_aps = FREEdoFreeTree( global_wlcomp_aps);
  }

  DBUG_RETURN ();
}


/** <!--********************************************************************-->
 *
 * @fn node *MakeIncDecLet( char *id, char *op)
 *
 *   @brief
 *   @param
 *   @return
 *
 ******************************************************************************/

static
node *MakeIncDecLet( char *name, char *op)
{
  node *let, *id, *ids, *ap;

  DBUG_ENTER ();
  ids = TBmakeSpids(  STRcpy( name), NULL);

  id = TBmakeSpid( NULL, name);

  ap = TBmakeSpap( TBmakeSpid( NULL, op),
                   TBmakeExprs( id, 
                     TBmakeExprs( TBmakeNum(1), NULL)));

  let = TBmakeLet( ids, ap);

  DBUG_RETURN (let);
}


/** <!--********************************************************************-->
 *
 * @fn node *MakeOpOnLet( char *id, node *expr, char *op)
 *
 *   @brief
 *   @param
 *   @return
 *
 ******************************************************************************/

static
node *MakeOpOnLet( char *name, node *expr, char *op)
{
  node *let, *id, *ids, *ap;

  DBUG_ENTER ();
  ids = TBmakeSpids( STRcpy( name), NULL);

  id = TBmakeSpid( NULL, name);

  ap = TBmakeSpap( TBmakeSpid( NULL, op),
                   TBmakeExprs( id,
                     TBmakeExprs( expr, NULL)));

  let = TBmakeLet( ids, ap);

  DBUG_RETURN (let);
}

/******************************************************************************
 *
 * Function:
 *   node *Expr2Mop( node * expr)
 *
 * Description:
 *
 ******************************************************************************/

static
node *Expr2Mop( node *expr)
{
  node *res;

  DBUG_ENTER ();

  if( (NODE_TYPE( expr) == N_spmop) && ! SPMOP_ISFIXED( expr) ) {
    res = expr;
  } else {
    res = TBmakeSpmop( NULL, TBmakeExprs( expr, NULL));
  }

  DBUG_RETURN (res);
}



/******************************************************************************
 *
 * Function:
 *   node *ConstructMop( node *, node *, node *)
 *
 * Description:
 *
 ******************************************************************************/

static
node *ConstructMop( node *expr1, node *fun_id, node *expr2)
{
  node *res, *lmop, *rmop, *fun_exprs;

  DBUG_ENTER ();

  lmop = Expr2Mop( expr1);
  rmop = Expr2Mop( expr2);

  fun_exprs = TBmakeExprs( fun_id, SPMOP_OPS( rmop));

  res = TBmakeSpmop( TCappendExprs( SPMOP_OPS( lmop),
                                    fun_exprs),
                     TCappendExprs( SPMOP_EXPRS( lmop),
                                    SPMOP_EXPRS( rmop)));
  /*
   * now we free the topmost node. Therefore we have to set the OPS and EXPRS
   * attributes to NULL, so that they do not get freed!
   */
  SPMOP_EXPRS( lmop) = NULL;
  SPMOP_OPS( lmop) = NULL;
  SPMOP_EXPRS( rmop) = NULL;
  SPMOP_OPS( rmop) = NULL;

  lmop = FREEdoFreeNode( lmop); 
  rmop = FREEdoFreeNode( rmop);

  DBUG_RETURN (res);
}



/******************************************************************************
 *
 * Function:
 *   node *CheckWlcompConf( node *conf, node *exprs)
 *
 * Description:
 *   Checks and converts the given wlcomp-pragma expression.
 *   Syntax of wlcomp-pragmas:
 *      pragma  ->  PRAGMA WLCOMP conf
 *      conf    ->  DEFAULT
 *               |  id BRACKET_L args conf BRACKET_R
 *      args    ->  $empty$
 *               |  arg COMMA args
 *      arg     ->  expr
 *      id      ->  ... identificator ...
 *      expr    ->  ... expression ...
 *   Examples:
 *      #pragma wlcomp Scheduling( Block(), Dynamic(),
 *                     BvL0( [3,3], [7,7],
 *                     Cubes(
 *                     Default)))
 *      #pragma wlcomp Conf3( ..., Conf2( ..., Conf1( ..., Default)))
 *   This nesting of applications is transformed into a N_exprs chain of
 *   wlcomp-pragma functions:
 *      Cubes()  ,  BvL0( [3,3], [7,7])  ,  Scheduling( Block(), Dynamic())
 *      Conf1(...)  ,  Conf2(...)  ,  Conf3(...)
 *
 *   Unfortunately, it is not possible to parse the wlcomp-pragma expression
 *   directly with yacc. 'conf' as well as 'arg' might start with an ID
 *   [ In the first example: Scheduling( Block(), ..., BvL0( ...)) ]
 *                                       ^^^^^         ^^^^
 *   This leads to an unsolvable shift/reduce conflict :-((
 *   Therefore 'conf' is simply parsed as an application/id and all the other
 *   stuff is done here.
 *
 ******************************************************************************/

static
node *CheckWlcompConf( node *conf, node *exprs)
{
  DBUG_ENTER ();

  DBUG_ASSERT (conf != NULL, "wlcomp-pragma is empty!");

  if (NODE_TYPE( conf) == N_spid) {
    if (!STReq( SPID_NAME( conf), "Default")) {
      strcpy( yytext, SPID_NAME( conf));
      yyerror( "innermost configuration is not 'Default'");
    }

    /*
     * free N_id node
     */
    conf = FREEdoFreeTree( conf);

    /*
     * unmodified 'exprs' is returned
     */
  }
  else if (NODE_TYPE( conf) == N_spap) {
    node *arg = SPAP_ARGS( conf);

    /*
     * look for last argument -> next 'conf'
     */
    if (arg == NULL) {
      strcpy( yytext, SPAP_NAME( conf));
      yyerror( "wlcomp-function with missing configuration found");
    }
    else {
      node *tmp;
      node *next_conf = NULL;

      if (EXPRS_NEXT( arg) == NULL) {
        next_conf = EXPRS_EXPR( arg);
        tmp = arg;
        AP_ARGS( conf) = NULL;
      }
      else {
        while (EXPRS_NEXT( EXPRS_NEXT( arg)) != NULL) {
          arg = EXPRS_NEXT( arg);
        }
        next_conf = EXPRS_EXPR( EXPRS_NEXT( arg));
        tmp = EXPRS_NEXT( arg);
        EXPRS_NEXT( arg) = NULL;
      }

      /*
       * free last N_exprs node
       */
      EXPRS_EXPR( tmp) = NULL;
      tmp = FREEdoFreeTree( tmp);

      if ((NODE_TYPE( next_conf) != N_spid) && (NODE_TYPE( next_conf) != N_spap)) {
        strcpy( yytext, AP_NAME( conf));
        yyerror( "wlcomp-function with illegal configuration found");
      }
      else {
        /*
         * insert new configuration at head of 'exprs'
         */
        exprs = TBmakeExprs( conf, exprs);
        exprs = CheckWlcompConf( next_conf, exprs);
      }
    }
  }
  else {
    DBUG_ASSERT (0, "wlcomp-pragma with illegal configuration found!");
  }

  DBUG_RETURN (exprs);
}

static int CountDotsInExprs( node *exprs)
{
  int result = 0;

  DBUG_ENTER ();

  while (exprs != NULL) {
    if (NODE_TYPE( EXPRS_EXPR( exprs)) == N_dot) {
      result++;
    }

    exprs = EXPRS_NEXT( exprs);
  }

  DBUG_RETURN (result);
}

static shape *Exprs2Shape( node *exprs)
{
  shape *result;
  int n;
  int cnt = 0;

  DBUG_ENTER ();
  
  n = TCcountExprs( exprs);

  result = SHmakeShape( n);

  while ((exprs != NULL) && (result != NULL)) {
    if (NODE_TYPE( EXPRS_EXPR( exprs)) == N_num) {
      result = SHsetExtent( result, cnt, NUM_VAL( EXPRS_EXPR( exprs)));
    } else {
      result = SHfreeShape( result);
    }
    exprs = EXPRS_NEXT( exprs);
    cnt++;
  }

  DBUG_RETURN (result);
}

static ntype *Exprs2NType( ntype *basetype, node *exprs)
{
  int n;
  int dots = 0;
  shape *shp;
  ntype *result = NULL;

  DBUG_ENTER ();

  n = TCcountExprs( exprs);

  switch (NODE_TYPE( EXPRS_EXPR1( exprs))) {
    case N_spid:
      if (SPID_NS( EXPRS_EXPR1( exprs)) != NULL) {
        yyerror("illegal shape specification");
      } else if (SPID_NAME( EXPRS_EXPR1( exprs))[1] != '\0') {
        yyerror("illegal shape specification");
      } else {
        switch (SPID_NAME( EXPRS_EXPR1( exprs))[0]) {
          case '*':
            result = TYmakeAUD( basetype);
            break;
          case '+':
            result = TYmakeAUDGZ( basetype);
            break;
          default:
            yyerror("illegal shape specification");
            break;
        }
      }
      break;
    case N_dot:
      dots = CountDotsInExprs( exprs);
      if (dots != n) {
        yyerror("illegal shape specification");
      } else {
        result = TYmakeAKD( basetype, dots, SHmakeShape(0));
      }
      break;
    case N_num:
      shp = Exprs2Shape( exprs);
      if (shp != NULL) {
        result = TYmakeAKS( basetype, shp);
      } else {
        yyerror("illegal shape specification");
      }
      break;
    default:
      yyerror("illegal shape specification");
      break;
  }

  exprs = FREEdoFreeTree( exprs);

  DBUG_RETURN (result);
}

static
node *SetClassType( node *module, ntype *type, node *pragmas)
{
  node *tdef;

  DBUG_ENTER ();

  tdef = TBmakeTypedef( 
           STRcpy( NSgetModule( MODULE_NAMESPACE( module))),
           NSdupNamespace( MODULE_NAMESPACE( module)),
           type,
           MODULE_TYPES( module));

  TYPEDEF_ISUNIQUE( tdef) = TRUE;
  TYPEDEF_PRAGMA( tdef) = pragmas;
  MODULE_TYPES( module) = tdef;

  DBUG_RETURN (module);
}
      
