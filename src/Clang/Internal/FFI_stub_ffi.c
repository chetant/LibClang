#include <inttypes.h>
#include <stdlib.h>
#include <clang-c/Index.h>
#include <stdio.h>
#include "src/Clang/Internal/FFI_stub_ffi.h"
HsPtr prim_createIndex(HsInt a,HsInt b)
{ HsPtr r;
  do { CXIndex r = clang_createIndex(a, b);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_availability_Available()
{ HsInt res1;
  do {res1=CXAvailability_Available;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_availability_Deprecated()
{ HsInt res1;
  do {res1=CXAvailability_Deprecated;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_availability_NotAvailable()
{ HsInt res1;
  do {res1=CXAvailability_NotAvailable;
      
      return((HsInt)(res1));} while(0);
}
 CXString * mkStrObj() { return malloc(sizeof(CXString)); }
  void freeStrObj(CXString * str) { clang_disposeString(*str);free(str); }
char * prim_getCString_Ptr(HsPtr d)
{ char * r;
  do { r = (char*) clang_getCString(*(CXString*)d);
      
      return((char *)(r));} while(0);
}
HsPtr prim_getFileName(HsPtr x)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getFileName(x);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getFile(HsPtr t,char * s)
{ HsPtr r;
  do { CXFile r = clang_getFile(t, s);
      
      return((HsPtr)(r));} while(0);
}
 void * srcLocListGetPtr(CXSourceLocation * s, int i, int pi) {return s[i].ptr_data[pi];}
  unsigned srcLocListGetData(CXSourceLocation * s, int i) {return s[i].int_data;}
void* prim_getNullLocation()
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceLocation r = clang_getNullLocation();
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getNullLocation_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getNullLocation_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getNullLocation_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
HsInt prim_equalLocations(HsPtr p1,HsPtr p2,HsInt d,HsPtr p12,HsPtr p22,HsInt d2)
{ do { CXSourceLocation l = {{p1, p2}, d};
     CXSourceLocation m = {{p12, p22}, d2};
      
      return((HsInt)(clang_equalLocations(l, m)));} while(0);
}
void* prim_getLocation(HsPtr t,HsPtr f,HsInt i,HsInt j)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceLocation r = clang_getLocation((CXTranslationUnit)t, f, i, j);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getLocation_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getLocation_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getLocation_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
void* prim_getLocationForOffset(HsPtr t,HsPtr f,HsInt i)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceLocation r = clang_getLocationForOffset((CXTranslationUnit)t, f, i);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getLocationForOffset_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getLocationForOffset_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getLocationForOffset_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
void* prim_getNullRange()
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;} gc_result;
  do { CXSourceRange r = clang_getNullRange();
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.begin_int_data);
      gc_result.gc_res4 = (HsInt)(r.end_int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getNullRange_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res1);}
HsPtr access_prim_getNullRange_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res2);}
HsInt access_prim_getNullRange_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res3);}
HsInt access_prim_getNullRange_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res4);}
void* prim_getRange(HsPtr p1,HsPtr p2,HsInt d,HsPtr p12,HsPtr p22,HsInt d2)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;} gc_result;
  do { CXSourceLocation l = {{p1, p2}, d};
     CXSourceLocation m = {{p12, p22}, d2};
     CXSourceRange r = clang_getRange(l, m);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.begin_int_data);
      gc_result.gc_res4 = (HsInt)(r.end_int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getRange_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res1);}
HsPtr access_prim_getRange_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res2);}
HsInt access_prim_getRange_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res3);}
HsInt access_prim_getRange_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res4);}
void* prim_getExpansionLocation(HsPtr p1,HsPtr p2,HsInt d)
{ static struct {HsPtr f;HsInt ln;HsInt c;HsInt o;} gc_result;
  HsPtr f; HsInt ln; HsInt c; HsInt o;
  do { CXSourceLocation l = {{p1, p2}, d};
     CXFile f;unsigned ln,c,o;clang_getExpansionLocation(l,&f,&ln,&c,&o);
      gc_result.f = (HsPtr)(f);
      gc_result.ln = (HsInt)(ln);
      gc_result.c = (HsInt)(c);
      gc_result.o = (HsInt)(o);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getExpansionLocation_f(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->f);}
HsInt access_prim_getExpansionLocation_ln(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->ln);}
HsInt access_prim_getExpansionLocation_c(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->c);}
HsInt access_prim_getExpansionLocation_o(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->o);}
void* prim_getInstantiationLocation(HsPtr p1,HsPtr p2,HsInt d)
{ static struct {HsPtr f;HsInt ln;HsInt c;HsInt o;} gc_result;
  HsPtr f; HsInt ln; HsInt c; HsInt o;
  do { CXSourceLocation l = {{p1, p2}, d};
     CXFile f;unsigned ln,c,o;clang_getInstantiationLocation(l,&f,&ln,&c,&o);
      gc_result.f = (HsPtr)(f);
      gc_result.ln = (HsInt)(ln);
      gc_result.c = (HsInt)(c);
      gc_result.o = (HsInt)(o);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getInstantiationLocation_f(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->f);}
HsInt access_prim_getInstantiationLocation_ln(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->ln);}
HsInt access_prim_getInstantiationLocation_c(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->c);}
HsInt access_prim_getInstantiationLocation_o(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->o);}
void* prim_getSpellingLocation(HsPtr p1,HsPtr p2,HsInt d)
{ static struct {HsPtr f;HsInt ln;HsInt c;HsInt o;} gc_result;
  HsPtr f; HsInt ln; HsInt c; HsInt o;
  do { CXSourceLocation l = {{p1, p2}, d};
     CXFile f;unsigned ln,c,o;clang_getSpellingLocation(l,&f,&ln,&c,&o);
      gc_result.f = (HsPtr)(f);
      gc_result.ln = (HsInt)(ln);
      gc_result.c = (HsInt)(c);
      gc_result.o = (HsInt)(o);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getSpellingLocation_f(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->f);}
HsInt access_prim_getSpellingLocation_ln(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->ln);}
HsInt access_prim_getSpellingLocation_c(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->c);}
HsInt access_prim_getSpellingLocation_o(void *ptr){ return(((struct {HsPtr f;HsInt ln;HsInt c;HsInt o;}*) ptr)->o);}
void* prim_getRangeStart(HsPtr p1,HsPtr p2,HsInt d1,HsInt d2)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceRange a = {{p1, p2}, d1, d2};
     CXSourceLocation r = clang_getRangeStart(a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getRangeStart_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getRangeStart_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getRangeStart_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
void* prim_getRangeEnd(HsPtr p1,HsPtr p2,HsInt d1,HsInt d2)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceRange a = {{p1, p2}, d1, d2};
     CXSourceLocation r = clang_getRangeEnd(a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getRangeEnd_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getRangeEnd_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getRangeEnd_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
HsInt prim_diagnostic_Ignored()
{ HsInt res1;
  do {res1=CXDiagnostic_Ignored;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_Note()
{ HsInt res1;
  do {res1=CXDiagnostic_Note;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_Warning()
{ HsInt res1;
  do {res1=CXDiagnostic_Warning;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_Error()
{ HsInt res1;
  do {res1=CXDiagnostic_Error;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_Fatal()
{ HsInt res1;
  do {res1=CXDiagnostic_Fatal;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getNumDiagnostics(HsPtr t)
{ HsInt r;
  do { unsigned r = clang_getNumDiagnostics((CXTranslationUnit)t);
      
      return((HsInt)(r));} while(0);
}
HsPtr prim_getDiagnostic(HsPtr t,HsInt i)
{ HsPtr r;
  do { CXDiagnostic r = clang_getDiagnostic((CXTranslationUnit)t, i);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_diagnostic_DisplaySourceLocation()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplaySourceLocation;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_DisplayColumn()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplayColumn;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_DisplaySourceRanges()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplaySourceRanges;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_DisplayOption()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplayOption;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_DisplayCategoryId()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplayCategoryId;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_diagnostic_DisplayCategoryName()
{ HsInt res1;
  do {res1=CXDiagnostic_DisplayCategoryName;
      
      return((HsInt)(res1));} while(0);
}
HsPtr prim_formatDiagnostic(HsPtr d,HsInt i)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_formatDiagnostic(d, i);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_defaultDiagnosticDisplayOptions()
{ HsInt res1;
  do {res1 = clang_defaultDiagnosticDisplayOptions();
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getDiagnosticSeverity(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getDiagnosticSeverity(arg1);
      
      return((HsInt)(res1));} while(0);
}
void* prim_getDiagnosticLocation(HsPtr d)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXSourceLocation r = clang_getDiagnosticLocation(d);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getDiagnosticLocation_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getDiagnosticLocation_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getDiagnosticLocation_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
HsPtr prim_getDiagnosticSpelling(HsPtr d)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getDiagnosticSpelling(d);
      
      return((HsPtr)(r));} while(0);
}
void* prim_getDiagnosticOption(HsPtr d)
{ static struct {HsPtr r;HsPtr a;} gc_result;
  HsPtr r; HsPtr a;
  do { CXString *a = mkStrObj();CXString *r = mkStrObj();*r = clang_getDiagnosticOption(d, a);
      gc_result.r = (HsPtr)(r);
      gc_result.a = (HsPtr)(a);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getDiagnosticOption_r(void *ptr){ return(((struct {HsPtr r;HsPtr a;}*) ptr)->r);}
HsPtr access_prim_getDiagnosticOption_a(void *ptr){ return(((struct {HsPtr r;HsPtr a;}*) ptr)->a);}
HsInt prim_getDiagnosticCategory(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getDiagnosticCategory(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsPtr prim_getDiagnosticCategoryText(HsPtr d)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getDiagnosticCategoryText(d);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_getDiagnosticNumRanges(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getDiagnosticNumRanges(arg1);
      
      return((HsInt)(res1));} while(0);
}
void* prim_getDiagnosticRange(HsPtr d,HsInt i)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;} gc_result;
  do { CXSourceRange r = clang_getDiagnosticRange(d, i);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.begin_int_data);
      gc_result.gc_res4 = (HsInt)(r.end_int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getDiagnosticRange_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res1);}
HsPtr access_prim_getDiagnosticRange_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res2);}
HsInt access_prim_getDiagnosticRange_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res3);}
HsInt access_prim_getDiagnosticRange_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res4);}
HsInt prim_getDiagnosticNumFixIts(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getDiagnosticNumFixIts(arg1);
      
      return((HsInt)(res1));} while(0);
}
void* prim_getDiagnosticFixIt(HsPtr d,HsInt i)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;} gc_result;
  HsPtr r;
  do { CXSourceRange a;CXString *r = mkStrObj();*r = clang_getDiagnosticFixIt(d, i, &a);
      gc_result.gc_res1 = (HsPtr)(a.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(a.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(a.begin_int_data);
      gc_result.gc_res4 = (HsInt)(a.end_int_data);
      gc_result.r = (HsPtr)(r);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getDiagnosticFixIt_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;}*) ptr)->gc_res1);}
HsPtr access_prim_getDiagnosticFixIt_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;}*) ptr)->gc_res2);}
HsInt access_prim_getDiagnosticFixIt_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;}*) ptr)->gc_res3);}
HsInt access_prim_getDiagnosticFixIt_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;}*) ptr)->gc_res4);}
HsPtr access_prim_getDiagnosticFixIt_r(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;HsPtr r;}*) ptr)->r);}
HsPtr prim_getTranslationUnitSpelling(HsPtr t)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getTranslationUnitSpelling(t);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_createTranslationUnitFromSourceFile(HsPtr i,char * s,HsPtr ss,HsInt ns,HsPtr ufs,HsInt nufs)
{ HsPtr r;
  do { r = clang_createTranslationUnitFromSourceFile(i,s,ns,ss,nufs,ufs);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_createTranslationUnit(HsPtr i,char * s)
{ HsPtr r;
  do { CXTranslationUnit r = clang_createTranslationUnit(i, s);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_translationUnit_None()
{ HsInt res1;
  do {res1=CXTranslationUnit_None;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_DetailedPreprocessingRecord()
{ HsInt res1;
  do {res1=CXTranslationUnit_DetailedPreprocessingRecord;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_Incomplete()
{ HsInt res1;
  do {res1=CXTranslationUnit_Incomplete;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_PrecompiledPreamble()
{ HsInt res1;
  do {res1=CXTranslationUnit_PrecompiledPreamble;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_CacheCompletionResults()
{ HsInt res1;
  do {res1=CXTranslationUnit_CacheCompletionResults;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_ForSerialization()
{ HsInt res1;
  do {res1=CXTranslationUnit_ForSerialization;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_CXXChainedPCH()
{ HsInt res1;
  do {res1=CXTranslationUnit_CXXChainedPCH;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_SkipFunctionBodies()
{ HsInt res1;
  do {res1=CXTranslationUnit_SkipFunctionBodies;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_translationUnit_IncludeBriefCommentsInCodeCompletion()
{ HsInt res1;
  do {res1=CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_defaultEditingTranslationUnitOptions()
{ HsInt res1;
  do {res1 = clang_defaultEditingTranslationUnitOptions();
      
      return((HsInt)(res1));} while(0);
}
HsPtr prim_parseTranslationUnit(HsPtr i,char * s,HsPtr ss,HsInt ns,HsPtr ufs,HsInt nufs,HsInt i2)
{ HsPtr r;
  do { r = clang_parseTranslationUnit(i,s,ss,ns,ufs,nufs,i2);
      
      return((HsPtr)(r));} while(0);
}
 uint32_t unsavedFileSize() { return sizeof(struct CXUnsavedFile); }
  void setCXUnsavedFile(char * filename, char * contents,unsigned long length, struct CXUnsavedFile* uf, int i) {uf[i].Filename=filename;uf[i].Contents=contents;uf[i].Length=length;}
HsInt prim_saveTranslationUnit_None()
{ HsInt res1;
  do {res1=CXSaveTranslationUnit_None;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_defaultSaveOptions(HsPtr t)
{ HsInt r;
  do { r = clang_defaultSaveOptions(t);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_saveTranslationUnit(HsPtr t,char * s,HsInt i)
{ do { unsigned r = clang_saveTranslationUnit(t, s, i);
      
      return((HsInt)(r!=0?0:1));} while(0);
}
HsInt prim_reparse_None()
{ HsInt res1;
  do {res1=CXReparse_None;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_defaultReparseOptions(HsPtr t)
{ HsInt r;
  do { r = clang_defaultReparseOptions(t);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_reparseTranslationUnit(HsPtr t,HsPtr ufs,HsInt nufs,HsInt i)
{ HsInt r;
  do { r = clang_reparseTranslationUnit(t, nufs, ufs, i);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_cursor_UnexposedDecl()
{ HsInt res1;
  do {res1=CXCursor_UnexposedDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_StructDecl()
{ HsInt res1;
  do {res1=CXCursor_StructDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnionDecl()
{ HsInt res1;
  do {res1=CXCursor_UnionDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ClassDecl()
{ HsInt res1;
  do {res1=CXCursor_ClassDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_EnumDecl()
{ HsInt res1;
  do {res1=CXCursor_EnumDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FieldDecl()
{ HsInt res1;
  do {res1=CXCursor_FieldDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_EnumConstantDecl()
{ HsInt res1;
  do {res1=CXCursor_EnumConstantDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FunctionDecl()
{ HsInt res1;
  do {res1=CXCursor_FunctionDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_VarDecl()
{ HsInt res1;
  do {res1=CXCursor_VarDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ParmDecl()
{ HsInt res1;
  do {res1=CXCursor_ParmDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCInterfaceDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCInterfaceDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCCategoryDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCCategoryDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCProtocolDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCProtocolDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCPropertyDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCPropertyDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCIvarDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCIvarDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCInstanceMethodDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCInstanceMethodDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCClassMethodDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCClassMethodDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCImplementationDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCImplementationDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCCategoryImplDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCCategoryImplDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TypedefDecl()
{ HsInt res1;
  do {res1=CXCursor_TypedefDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXMethod()
{ HsInt res1;
  do {res1=CXCursor_CXXMethod;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_Namespace()
{ HsInt res1;
  do {res1=CXCursor_Namespace;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LinkageSpec()
{ HsInt res1;
  do {res1=CXCursor_LinkageSpec;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_Constructor()
{ HsInt res1;
  do {res1=CXCursor_Constructor;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_Destructor()
{ HsInt res1;
  do {res1=CXCursor_Destructor;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ConversionFunction()
{ HsInt res1;
  do {res1=CXCursor_ConversionFunction;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TemplateTypeParameter()
{ HsInt res1;
  do {res1=CXCursor_TemplateTypeParameter;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NonTypeTemplateParameter()
{ HsInt res1;
  do {res1=CXCursor_NonTypeTemplateParameter;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TemplateTemplateParameter()
{ HsInt res1;
  do {res1=CXCursor_TemplateTemplateParameter;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FunctionTemplate()
{ HsInt res1;
  do {res1=CXCursor_FunctionTemplate;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ClassTemplate()
{ HsInt res1;
  do {res1=CXCursor_ClassTemplate;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ClassTemplatePartialSpecialization()
{ HsInt res1;
  do {res1=CXCursor_ClassTemplatePartialSpecialization;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NamespaceAlias()
{ HsInt res1;
  do {res1=CXCursor_NamespaceAlias;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UsingDirective()
{ HsInt res1;
  do {res1=CXCursor_UsingDirective;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UsingDeclaration()
{ HsInt res1;
  do {res1=CXCursor_UsingDeclaration;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TypeAliasDecl()
{ HsInt res1;
  do {res1=CXCursor_TypeAliasDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCSynthesizeDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCSynthesizeDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCDynamicDecl()
{ HsInt res1;
  do {res1=CXCursor_ObjCDynamicDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXAccessSpecifier()
{ HsInt res1;
  do {res1=CXCursor_CXXAccessSpecifier;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstDecl()
{ HsInt res1;
  do {res1=CXCursor_FirstDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastDecl()
{ HsInt res1;
  do {res1=CXCursor_LastDecl;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstRef()
{ HsInt res1;
  do {res1=CXCursor_FirstRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCSuperClassRef()
{ HsInt res1;
  do {res1=CXCursor_ObjCSuperClassRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCProtocolRef()
{ HsInt res1;
  do {res1=CXCursor_ObjCProtocolRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCClassRef()
{ HsInt res1;
  do {res1=CXCursor_ObjCClassRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TypeRef()
{ HsInt res1;
  do {res1=CXCursor_TypeRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXBaseSpecifier()
{ HsInt res1;
  do {res1=CXCursor_CXXBaseSpecifier;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TemplateRef()
{ HsInt res1;
  do {res1=CXCursor_TemplateRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NamespaceRef()
{ HsInt res1;
  do {res1=CXCursor_NamespaceRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_MemberRef()
{ HsInt res1;
  do {res1=CXCursor_MemberRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LabelRef()
{ HsInt res1;
  do {res1=CXCursor_LabelRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_OverloadedDeclRef()
{ HsInt res1;
  do {res1=CXCursor_OverloadedDeclRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastRef()
{ HsInt res1;
  do {res1=CXCursor_LastRef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstInvalid()
{ HsInt res1;
  do {res1=CXCursor_FirstInvalid;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_InvalidFile()
{ HsInt res1;
  do {res1=CXCursor_InvalidFile;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NoDeclFound()
{ HsInt res1;
  do {res1=CXCursor_NoDeclFound;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NotImplemented()
{ HsInt res1;
  do {res1=CXCursor_NotImplemented;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_InvalidCode()
{ HsInt res1;
  do {res1=CXCursor_InvalidCode;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastInvalid()
{ HsInt res1;
  do {res1=CXCursor_LastInvalid;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstExpr()
{ HsInt res1;
  do {res1=CXCursor_FirstExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnexposedExpr()
{ HsInt res1;
  do {res1=CXCursor_UnexposedExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_DeclRefExpr()
{ HsInt res1;
  do {res1=CXCursor_DeclRefExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_MemberRefExpr()
{ HsInt res1;
  do {res1=CXCursor_MemberRefExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CallExpr()
{ HsInt res1;
  do {res1=CXCursor_CallExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCMessageExpr()
{ HsInt res1;
  do {res1=CXCursor_ObjCMessageExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_BlockExpr()
{ HsInt res1;
  do {res1=CXCursor_BlockExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IntegerLiteral()
{ HsInt res1;
  do {res1=CXCursor_IntegerLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FloatingLiteral()
{ HsInt res1;
  do {res1=CXCursor_FloatingLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ImaginaryLiteral()
{ HsInt res1;
  do {res1=CXCursor_ImaginaryLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_StringLiteral()
{ HsInt res1;
  do {res1=CXCursor_StringLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CharacterLiteral()
{ HsInt res1;
  do {res1=CXCursor_CharacterLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ParenExpr()
{ HsInt res1;
  do {res1=CXCursor_ParenExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnaryOperator()
{ HsInt res1;
  do {res1=CXCursor_UnaryOperator;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ArraySubscriptExpr()
{ HsInt res1;
  do {res1=CXCursor_ArraySubscriptExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_BinaryOperator()
{ HsInt res1;
  do {res1=CXCursor_BinaryOperator;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CompoundAssignOperator()
{ HsInt res1;
  do {res1=CXCursor_CompoundAssignOperator;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ConditionalOperator()
{ HsInt res1;
  do {res1=CXCursor_ConditionalOperator;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CStyleCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CStyleCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CompoundLiteralExpr()
{ HsInt res1;
  do {res1=CXCursor_CompoundLiteralExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_InitListExpr()
{ HsInt res1;
  do {res1=CXCursor_InitListExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_AddrLabelExpr()
{ HsInt res1;
  do {res1=CXCursor_AddrLabelExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_StmtExpr()
{ HsInt res1;
  do {res1=CXCursor_StmtExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_GenericSelectionExpr()
{ HsInt res1;
  do {res1=CXCursor_GenericSelectionExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_GNUNullExpr()
{ HsInt res1;
  do {res1=CXCursor_GNUNullExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXStaticCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXStaticCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXDynamicCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXDynamicCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXReinterpretCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXReinterpretCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXConstCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXConstCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXFunctionalCastExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXFunctionalCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXTypeidExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXTypeidExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXBoolLiteralExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXBoolLiteralExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXNullPtrLiteralExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXNullPtrLiteralExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXThisExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXThisExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXThrowExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXThrowExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXNewExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXNewExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXDeleteExpr()
{ HsInt res1;
  do {res1=CXCursor_CXXDeleteExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnaryExpr()
{ HsInt res1;
  do {res1=CXCursor_UnaryExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCStringLiteral()
{ HsInt res1;
  do {res1=CXCursor_ObjCStringLiteral;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCEncodeExpr()
{ HsInt res1;
  do {res1=CXCursor_ObjCEncodeExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCSelectorExpr()
{ HsInt res1;
  do {res1=CXCursor_ObjCSelectorExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCProtocolExpr()
{ HsInt res1;
  do {res1=CXCursor_ObjCProtocolExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCBridgedCastExpr()
{ HsInt res1;
  do {res1=CXCursor_ObjCBridgedCastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_PackExpansionExpr()
{ HsInt res1;
  do {res1=CXCursor_PackExpansionExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_SizeOfPackExpr()
{ HsInt res1;
  do {res1=CXCursor_SizeOfPackExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastExpr()
{ HsInt res1;
  do {res1=CXCursor_LastExpr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstStmt()
{ HsInt res1;
  do {res1=CXCursor_FirstStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnexposedStmt()
{ HsInt res1;
  do {res1=CXCursor_UnexposedStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LabelStmt()
{ HsInt res1;
  do {res1=CXCursor_LabelStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CompoundStmt()
{ HsInt res1;
  do {res1=CXCursor_CompoundStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CaseStmt()
{ HsInt res1;
  do {res1=CXCursor_CaseStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_DefaultStmt()
{ HsInt res1;
  do {res1=CXCursor_DefaultStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IfStmt()
{ HsInt res1;
  do {res1=CXCursor_IfStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_SwitchStmt()
{ HsInt res1;
  do {res1=CXCursor_SwitchStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_WhileStmt()
{ HsInt res1;
  do {res1=CXCursor_WhileStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_DoStmt()
{ HsInt res1;
  do {res1=CXCursor_DoStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ForStmt()
{ HsInt res1;
  do {res1=CXCursor_ForStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_GotoStmt()
{ HsInt res1;
  do {res1=CXCursor_GotoStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IndirectGotoStmt()
{ HsInt res1;
  do {res1=CXCursor_IndirectGotoStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ContinueStmt()
{ HsInt res1;
  do {res1=CXCursor_ContinueStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_BreakStmt()
{ HsInt res1;
  do {res1=CXCursor_BreakStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ReturnStmt()
{ HsInt res1;
  do {res1=CXCursor_ReturnStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_AsmStmt()
{ HsInt res1;
  do {res1=CXCursor_AsmStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAtTryStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAtTryStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAtCatchStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAtCatchStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAtFinallyStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAtFinallyStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAtThrowStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAtThrowStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAtSynchronizedStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAtSynchronizedStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCAutoreleasePoolStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCAutoreleasePoolStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_ObjCForCollectionStmt()
{ HsInt res1;
  do {res1=CXCursor_ObjCForCollectionStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXCatchStmt()
{ HsInt res1;
  do {res1=CXCursor_CXXCatchStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXTryStmt()
{ HsInt res1;
  do {res1=CXCursor_CXXTryStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXForRangeStmt()
{ HsInt res1;
  do {res1=CXCursor_CXXForRangeStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_SEHTryStmt()
{ HsInt res1;
  do {res1=CXCursor_SEHTryStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_SEHExceptStmt()
{ HsInt res1;
  do {res1=CXCursor_SEHExceptStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_SEHFinallyStmt()
{ HsInt res1;
  do {res1=CXCursor_SEHFinallyStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_NullStmt()
{ HsInt res1;
  do {res1=CXCursor_NullStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_DeclStmt()
{ HsInt res1;
  do {res1=CXCursor_DeclStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastStmt()
{ HsInt res1;
  do {res1=CXCursor_LastStmt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_TranslationUnit()
{ HsInt res1;
  do {res1=CXCursor_TranslationUnit;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstAttr()
{ HsInt res1;
  do {res1=CXCursor_FirstAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_UnexposedAttr()
{ HsInt res1;
  do {res1=CXCursor_UnexposedAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IBActionAttr()
{ HsInt res1;
  do {res1=CXCursor_IBActionAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IBOutletAttr()
{ HsInt res1;
  do {res1=CXCursor_IBOutletAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_IBOutletCollectionAttr()
{ HsInt res1;
  do {res1=CXCursor_IBOutletCollectionAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXFinalAttr()
{ HsInt res1;
  do {res1=CXCursor_CXXFinalAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_CXXOverrideAttr()
{ HsInt res1;
  do {res1=CXCursor_CXXOverrideAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_AnnotateAttr()
{ HsInt res1;
  do {res1=CXCursor_AnnotateAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastAttr()
{ HsInt res1;
  do {res1=CXCursor_LastAttr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_PreprocessingDirective()
{ HsInt res1;
  do {res1=CXCursor_PreprocessingDirective;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_MacroDefinition()
{ HsInt res1;
  do {res1=CXCursor_MacroDefinition;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_MacroExpansion()
{ HsInt res1;
  do {res1=CXCursor_MacroExpansion;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_MacroInstantiation()
{ HsInt res1;
  do {res1=CXCursor_MacroInstantiation;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_InclusionDirective()
{ HsInt res1;
  do {res1=CXCursor_InclusionDirective;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_FirstPreprocessing()
{ HsInt res1;
  do {res1=CXCursor_FirstPreprocessing;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cursor_LastPreprocessing()
{ HsInt res1;
  do {res1=CXCursor_LastPreprocessing;
      
      return((HsInt)(res1));} while(0);
}
 unsigned sizeOfCXCursor() {return sizeof(CXCursor);}
  void * getCXCursorData(CXCursor * p, int i){return p->data[i];}
  enum CXCursorKind getCXCursorKind(CXCursor * p){return p->kind;}
  void setCXCursorData(CXCursor * p, int i, void * ptr){p->data[i] = ptr;}
  void setCXCursorKind(CXCursor * p, unsigned kind){p->kind = kind;}
  unsigned getCXCursorXData(CXCursor * p){return p->xdata;}
  void setCXCursorXData(CXCursor * p, unsigned i){p->xdata = i;}
void* prim_getNullCursor()
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor r = clang_getNullCursor();
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getNullCursor_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getNullCursor_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getNullCursor_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getNullCursor_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getNullCursor_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
void* prim_getTranslationUnitCursor(HsPtr t)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor r = clang_getTranslationUnitCursor(t);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getTranslationUnitCursor_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getTranslationUnitCursor_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getTranslationUnitCursor_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getTranslationUnitCursor_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getTranslationUnitCursor_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
HsInt prim_equalCursors(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3,HsInt k2,HsInt xdata2,HsPtr p12,HsPtr p22,HsPtr p32)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor b = {k2, xdata2, {p12, p22, p32}};
     unsigned r = clang_equalCursors(a, b);
      
      return((HsInt)(r));} while(0);
}
HsWord32 prim_hashCursor(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsWord32 r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     unsigned r = clang_hashCursor(a);
      
      return((HsWord32)(r));} while(0);
}
HsInt prim_getCursorKind(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     enum CXCursorKind r = clang_getCursorKind(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_isDeclaration(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isDeclaration(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isReference(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isReference(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isExpression(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isExpression(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isStatement(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isStatement(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isInvalid(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isInvalid(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isTranslationUnit(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isTranslationUnit(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isPreprocessing(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isPreprocessing(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_isUnexposed(HsInt arg1)
{ HsInt res1;
  do {res1 = clang_isUnexposed(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_linkage_Invalid()
{ HsInt res1;
  do {res1=CXLinkage_Invalid;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_linkage_NoLinkage()
{ HsInt res1;
  do {res1=CXLinkage_NoLinkage;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_linkage_Internal()
{ HsInt res1;
  do {res1=CXLinkage_Internal;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_linkage_UniqueExternal()
{ HsInt res1;
  do {res1=CXLinkage_UniqueExternal;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_linkage_External()
{ HsInt res1;
  do {res1=CXLinkage_External;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCursorLinkage(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     enum CXLinkageKind r = clang_getCursorLinkage(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_getCursorAvailability(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     enum CXAvailabilityKind r = clang_getCursorAvailability(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_language_Invalid()
{ HsInt res1;
  do {res1=CXLanguage_Invalid;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_language_C()
{ HsInt res1;
  do {res1=CXLanguage_C;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_language_ObjC()
{ HsInt res1;
  do {res1=CXLanguage_ObjC;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_language_CPlusPlus()
{ HsInt res1;
  do {res1=CXLanguage_CPlusPlus;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCursorLanguage(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     enum CXLanguageKind r = clang_getCursorLanguage(a);
      
      return((HsInt)(r));} while(0);
}
HsPtr prim_createCXCursorSet()
{ HsPtr r;
  do { CXCursorSet r = clang_createCXCursorSet();
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_cXCursorSet_contains(HsPtr cs,HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ do { CXCursor a = {k, xdata, {p1, p2, p3}};
      
      return((HsInt)(clang_CXCursorSet_contains(cs, a)));} while(0);
}
HsInt prim_cXCursorSet_insert(HsPtr cs,HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ do { CXCursor a = {k, xdata, {p1, p2, p3}};
      
      return((HsInt)(clang_CXCursorSet_insert(cs, a)));} while(0);
}
void* prim_getCursorSemanticParent(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getCursorSemanticParent(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorSemanticParent_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorSemanticParent_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorSemanticParent_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCursorSemanticParent_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCursorSemanticParent_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
void* prim_getCursorLexicalParent(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getCursorLexicalParent(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorLexicalParent_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorLexicalParent_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorLexicalParent_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCursorLexicalParent_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCursorLexicalParent_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
 enum CXCursorKind cursorListGetKind(CXCursor *clist, int i) {return clist[i].kind;}
  unsigned cursorListGetXData(CXCursor *clist, int i) {return clist[i].xdata;}
  void * cursorListGetPtr(CXCursor *clist, int i, int pi) {return clist[i].data[pi];}
void* prim_getOverriddenCursors(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt num_overrides;HsPtr overrides;} gc_result;
  HsInt num_overrides; HsPtr overrides;
  do { CXCursor a = {k , xdata, {p1, p2, p3}};
     CXCursor * overrides;unsigned num_overrides;
     clang_getOverriddenCursors(a, &overrides, &num_overrides);
      gc_result.num_overrides = (HsInt)(num_overrides);
      gc_result.overrides = (HsPtr)(overrides);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getOverriddenCursors_num_overrides(void *ptr){ return(((struct {HsInt num_overrides;HsPtr overrides;}*) ptr)->num_overrides);}
HsPtr access_prim_getOverriddenCursors_overrides(void *ptr){ return(((struct {HsInt num_overrides;HsPtr overrides;}*) ptr)->overrides);}
HsPtr prim_getIncludedFile(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ do { CXCursor a = {k, xdata, {p1, p2, p3}};
      
      return((HsPtr)(clang_getIncludedFile(a)));} while(0);
}
void* prim_getCursor(HsPtr t,HsPtr p1,HsPtr p2,HsInt d)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXSourceLocation l = {{p1, p2}, d};
     CXCursor r = clang_getCursor(t, l);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursor_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCursor_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursor_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCursor_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCursor_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
void* prim_getCursorLocation(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXSourceLocation r = clang_getCursorLocation(a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getCursorLocation_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getCursorLocation_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorLocation_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
void* prim_getCursorExtent(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXSourceRange r = clang_getCursorExtent(a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.begin_int_data);
      gc_result.gc_res4 = (HsInt)(r.end_int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getCursorExtent_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res1);}
HsPtr access_prim_getCursorExtent_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorExtent_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res3);}
HsInt access_prim_getCursorExtent_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res4);}
HsInt prim_type_Invalid()
{ HsInt res1;
  do {res1=CXType_Invalid;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Unexposed()
{ HsInt res1;
  do {res1=CXType_Unexposed;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Void()
{ HsInt res1;
  do {res1=CXType_Void;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Bool()
{ HsInt res1;
  do {res1=CXType_Bool;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Char_U()
{ HsInt res1;
  do {res1=CXType_Char_U;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_UChar()
{ HsInt res1;
  do {res1=CXType_UChar;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Char16()
{ HsInt res1;
  do {res1=CXType_Char16;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Char32()
{ HsInt res1;
  do {res1=CXType_Char32;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_UShort()
{ HsInt res1;
  do {res1=CXType_UShort;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_UInt()
{ HsInt res1;
  do {res1=CXType_UInt;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ULong()
{ HsInt res1;
  do {res1=CXType_ULong;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ULongLong()
{ HsInt res1;
  do {res1=CXType_ULongLong;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_UInt128()
{ HsInt res1;
  do {res1=CXType_UInt128;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Char_S()
{ HsInt res1;
  do {res1=CXType_Char_S;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_SChar()
{ HsInt res1;
  do {res1=CXType_SChar;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_WChar()
{ HsInt res1;
  do {res1=CXType_WChar;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Short()
{ HsInt res1;
  do {res1=CXType_Short;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Int()
{ HsInt res1;
  do {res1=CXType_Int;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Long()
{ HsInt res1;
  do {res1=CXType_Long;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_LongLong()
{ HsInt res1;
  do {res1=CXType_LongLong;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Int128()
{ HsInt res1;
  do {res1=CXType_Int128;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Float()
{ HsInt res1;
  do {res1=CXType_Float;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Double()
{ HsInt res1;
  do {res1=CXType_Double;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_LongDouble()
{ HsInt res1;
  do {res1=CXType_LongDouble;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_NullPtr()
{ HsInt res1;
  do {res1=CXType_NullPtr;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Overload()
{ HsInt res1;
  do {res1=CXType_Overload;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Dependent()
{ HsInt res1;
  do {res1=CXType_Dependent;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ObjCId()
{ HsInt res1;
  do {res1=CXType_ObjCId;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ObjCClass()
{ HsInt res1;
  do {res1=CXType_ObjCClass;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ObjCSel()
{ HsInt res1;
  do {res1=CXType_ObjCSel;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_FirstBuiltin()
{ HsInt res1;
  do {res1=CXType_FirstBuiltin;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_LastBuiltin()
{ HsInt res1;
  do {res1=CXType_LastBuiltin;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Complex()
{ HsInt res1;
  do {res1=CXType_Complex;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Pointer()
{ HsInt res1;
  do {res1=CXType_Pointer;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_BlockPointer()
{ HsInt res1;
  do {res1=CXType_BlockPointer;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_LValueReference()
{ HsInt res1;
  do {res1=CXType_LValueReference;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_RValueReference()
{ HsInt res1;
  do {res1=CXType_RValueReference;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Record()
{ HsInt res1;
  do {res1=CXType_Record;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Enum()
{ HsInt res1;
  do {res1=CXType_Enum;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_Typedef()
{ HsInt res1;
  do {res1=CXType_Typedef;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ObjCInterface()
{ HsInt res1;
  do {res1=CXType_ObjCInterface;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ObjCObjectPointer()
{ HsInt res1;
  do {res1=CXType_ObjCObjectPointer;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_FunctionNoProto()
{ HsInt res1;
  do {res1=CXType_FunctionNoProto;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_FunctionProto()
{ HsInt res1;
  do {res1=CXType_FunctionProto;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_type_ConstantArray()
{ HsInt res1;
  do {res1=CXType_ConstantArray;
      
      return((HsInt)(res1));} while(0);
}
void* prim_getCursorType(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXType r = clang_getCursorType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getCursorType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
HsInt prim_equalTypes(HsInt k,HsPtr p1,HsPtr p2,HsInt k2,HsPtr p12,HsPtr p22)
{ HsInt r;
  do { CXType a = {k, {p1, p2}};
     CXType b = {k2, {p12, p22}};
     r = clang_equalTypes(a, b);
      
      return((HsInt)(r));} while(0);
}
void* prim_getCanonicalType(HsInt k,HsPtr p1,HsPtr p2)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXType a = {k, {p1, p2}};
     CXType r = clang_getCanonicalType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCanonicalType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getCanonicalType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getCanonicalType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
HsInt prim_isConstQualifiedType(HsInt k,HsPtr p1,HsPtr p2)
{ HsInt r;
  do { CXType a = {k, {p1, p2}};
     r = clang_isConstQualifiedType(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_isVolatileQualifiedType(HsInt k,HsPtr p1,HsPtr p2)
{ HsInt r;
  do { CXType a = {k, {p1, p2}};
     r = clang_isVolatileQualifiedType(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_isRestrictQualifiedType(HsInt k,HsPtr p1,HsPtr p2)
{ HsInt r;
  do { CXType a = {k, {p1, p2}};
     r = clang_isRestrictQualifiedType(a);
      
      return((HsInt)(r));} while(0);
}
void* prim_getPointeeType(HsInt k,HsPtr p1,HsPtr p2)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXType a = {k, {p1, p2}};
     CXType r = clang_getPointeeType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getPointeeType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getPointeeType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getPointeeType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
void* prim_getTypeDeclaration(HsInt k,HsPtr p1,HsPtr p2)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXType a = {k, {p1, p2}};
     CXCursor r = clang_getTypeDeclaration(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getTypeDeclaration_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getTypeDeclaration_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getTypeDeclaration_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getTypeDeclaration_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getTypeDeclaration_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
HsPtr prim_getDeclObjCTypeEncoding(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsPtr r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXString *r = mkStrObj();*r = clang_getDeclObjCTypeEncoding(a);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getTypeKindSpelling(HsInt tk)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getTypeKindSpelling(tk);
      
      return((HsPtr)(r));} while(0);
}
void* prim_getResultType(HsInt k,HsPtr p1,HsPtr p2)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXType a = {k, {p1, p2}};
     CXType r = clang_getResultType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getResultType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getResultType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getResultType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
void* prim_getCursorResultType(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXType r = clang_getCursorResultType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorResultType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getCursorResultType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorResultType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
HsInt prim_isPODType(HsInt k,HsPtr p1,HsPtr p2)
{ HsInt r;
  do { CXType a = {k, {p1, p2}};
     r = clang_isPODType(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_isVirtualBase(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_isVirtualBase(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_cXXInvalidAccessSpecifier()
{ HsInt res1;
  do {res1=CX_CXXInvalidAccessSpecifier;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cXXPublic()
{ HsInt res1;
  do {res1=CX_CXXPublic;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cXXProtected()
{ HsInt res1;
  do {res1=CX_CXXProtected;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_cXXPrivate()
{ HsInt res1;
  do {res1=CX_CXXPrivate;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCXXAccessSpecifier(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_getCXXAccessSpecifier(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_getNumOverloadedDecls(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_getNumOverloadedDecls(a);
      
      return((HsInt)(r));} while(0);
}
void* prim_getOverloadedDecl(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3,HsInt i)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getOverloadedDecl(a, i);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getOverloadedDecl_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getOverloadedDecl_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getOverloadedDecl_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getOverloadedDecl_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getOverloadedDecl_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
void* prim_getIBOutletCollectionType(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXType r = clang_getIBOutletCollectionType(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsPtr)(r.data[0]);
      gc_result.gc_res4 = (HsPtr)(r.data[1]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getIBOutletCollectionType_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res2);}
HsPtr access_prim_getIBOutletCollectionType_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res3);}
HsPtr access_prim_getIBOutletCollectionType_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsPtr gc_res3;HsPtr gc_res4;}*) ptr)->gc_res4);}
HsInt prim_childVisit_Break()
{ HsInt res1;
  do {res1=CXChildVisit_Break;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_childVisit_Continue()
{ HsInt res1;
  do {res1=CXChildVisit_Continue;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_childVisit_Recurse()
{ HsInt res1;
  do {res1=CXChildVisit_Recurse;
      
      return((HsInt)(res1));} while(0);
}
 typedef enum CXChildVisitResult (*HSCursorVisitor)
     (HsInt ck,HsInt cxdata, HsPtr cp1, HsPtr cp2,HsPtr cp3,
      HsInt pk,HsInt pxdata, HsPtr pp1, HsPtr pp2,HsPtr pp3);

  enum CXChildVisitResult primChildVisitor(CXCursor c, CXCursor p, CXClientData visitorp)
  {
    HSCursorVisitor visitor = (HSCursorVisitor) visitorp;
    return visitor(c.kind, c.xdata, c.data[0], c.data[1], c.data[2], 
                   p.kind, c.xdata, p.data[0], p.data[1], p.data[2]);
  }

  unsigned prim_visitChildren_(HsInt ck, HsInt cxdata, HsPtr p1, HsPtr p2, HsPtr p3, HsPtr fp)
  {
    CXCursor p = {ck, cxdata, {p1,p2,p3}};
    return clang_visitChildren(p, primChildVisitor, (CXClientData) fp);
  }
HsPtr prim_getCursorUSR(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsPtr r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXString *r = mkStrObj();*r = clang_getCursorUSR(a);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCClass(char * s)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCClass(s);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCCategory(char * s,char * p)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCCategory(s, p);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCProtocol(char * s)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCProtocol(s);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCIvar_Ptr(char * s,HsPtr x)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCIvar(s, *(CXString *)x);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCMethod_Ptr(char * s,HsInt b,HsPtr x)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCMethod(s, b, *(CXString *)x);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_constructUSR_ObjCProperty_Ptr(char * s,HsPtr x)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_constructUSR_ObjCProperty(s, *(CXString *)x);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getCursorSpelling(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsPtr r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXString *r = mkStrObj();*r = clang_getCursorSpelling(a);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getCursorDisplayName(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsPtr r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXString *r = mkStrObj();*r = clang_getCursorDisplayName(a);
      
      return((HsPtr)(r));} while(0);
}
void* prim_getCursorReferenced(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getCursorReferenced(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorReferenced_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorReferenced_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorReferenced_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCursorReferenced_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCursorReferenced_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
void* prim_getCursorDefinition(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getCursorDefinition(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCursorDefinition_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCursorDefinition_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCursorDefinition_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCursorDefinition_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCursorDefinition_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
HsInt prim_isCursorDefinition(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_isCursorDefinition(a);
      
      return((HsInt)(r));} while(0);
}
void* prim_getCanonicalCursor(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getCanonicalCursor(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getCanonicalCursor_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getCanonicalCursor_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getCanonicalCursor_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getCanonicalCursor_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getCanonicalCursor_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
HsInt prim_cXXMethod_isStatic(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_CXXMethod_isStatic(a);
      
      return((HsInt)(r));} while(0);
}
HsInt prim_getTemplateCursorKind(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ HsInt r;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     r = clang_getTemplateCursorKind(a);
      
      return((HsInt)(r));} while(0);
}
void* prim_getSpecializedCursorTemplate(HsInt k,HsInt xdata,HsPtr p1,HsPtr p2,HsPtr p3)
{ static struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;} gc_result;
  do { CXCursor a = {k, xdata, {p1, p2, p3}};
     CXCursor r = clang_getSpecializedCursorTemplate(a);
      gc_result.gc_res2 = (HsInt)(r.kind);
      gc_result.gc_res3 = (HsInt)(r.xdata);
      gc_result.gc_res4 = (HsPtr)(r.data[0]);
      gc_result.gc_res5 = (HsPtr)(r.data[1]);
      gc_result.gc_res6 = (HsPtr)(r.data[2]);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_getSpecializedCursorTemplate_gc_res2(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res2);}
HsInt access_prim_getSpecializedCursorTemplate_gc_res3(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res3);}
HsPtr access_prim_getSpecializedCursorTemplate_gc_res4(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res4);}
HsPtr access_prim_getSpecializedCursorTemplate_gc_res5(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res5);}
HsPtr access_prim_getSpecializedCursorTemplate_gc_res6(void *ptr){ return(((struct {HsInt gc_res2;HsInt gc_res3;HsPtr gc_res4;HsPtr gc_res5;HsPtr gc_res6;}*) ptr)->gc_res6);}
HsInt prim_token_Punctuation()
{ HsInt res1;
  do {res1=CXToken_Punctuation;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_token_Keyword()
{ HsInt res1;
  do {res1=CXToken_Keyword;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_token_Identifier()
{ HsInt res1;
  do {res1=CXToken_Identifier;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_token_Literal()
{ HsInt res1;
  do {res1=CXToken_Literal;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_token_Comment()
{ HsInt res1;
  do {res1=CXToken_Comment;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getTokenKind(HsInt w,HsInt x,HsInt y,HsInt z,HsPtr p)
{ HsInt r;
  do { CXToken a = {{w, x, y, z}, p};
     r = clang_getTokenKind(a);
      
      return((HsInt)(r));} while(0);
}
HsPtr prim_getTokenSpelling(HsPtr t,HsInt w,HsInt x,HsInt y,HsInt z,HsPtr p)
{ HsPtr r;
  do { CXToken a = {{w, x, y, z}, p};
     CXString *r = mkStrObj();*r = clang_getTokenSpelling(t, a);
      
      return((HsPtr)(r));} while(0);
}
void* prim_getTokenLocation(HsPtr t,HsInt w,HsInt x,HsInt y,HsInt z,HsPtr p)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;} gc_result;
  do { CXToken a = {{w, x, y, z}, p};
     CXSourceLocation r = clang_getTokenLocation(t, a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getTokenLocation_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res1);}
HsPtr access_prim_getTokenLocation_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res2);}
HsInt access_prim_getTokenLocation_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;}*) ptr)->gc_res3);}
void* prim_getTokenExtent(HsPtr t,HsInt w,HsInt x,HsInt y,HsInt z,HsPtr p)
{ static struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;} gc_result;
  do { CXToken a = {{w, x, y, z}, p};
     CXSourceRange r = clang_getTokenExtent(t, a);
      gc_result.gc_res1 = (HsPtr)(r.ptr_data[0]);
      gc_result.gc_res2 = (HsPtr)(r.ptr_data[1]);
      gc_result.gc_res3 = (HsInt)(r.begin_int_data);
      gc_result.gc_res4 = (HsInt)(r.end_int_data);
      
      return(&gc_result);} while(0);
}
HsPtr access_prim_getTokenExtent_gc_res1(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res1);}
HsPtr access_prim_getTokenExtent_gc_res2(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res2);}
HsInt access_prim_getTokenExtent_gc_res3(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res3);}
HsInt access_prim_getTokenExtent_gc_res4(void *ptr){ return(((struct {HsPtr gc_res1;HsPtr gc_res2;HsInt gc_res3;HsInt gc_res4;}*) ptr)->gc_res4);}
 unsigned tokenListGetInt(CXToken *tlist, int i, int pi) {return tlist[i].int_data[pi];}
  void * tokenListGetPtr(CXToken *tlist, int i) {return tlist[i].ptr_data;}
  void * makeTokens(int n) { return malloc(n * sizeof(CXToken)); }
  void freeTokens(void * tlist) { free(tlist); }
  void setTokenList(CXToken *tlist,int i,int w,int x,int y,int z,void *p) {CXToken a = {{w, x, y, z}, p};tlist[i]=a;}
void* prim_tokenize(HsPtr t,HsPtr p1,HsPtr p2,HsInt d1,HsInt d2)
{ static struct {HsInt numTokens;HsPtr tokens;} gc_result;
  HsInt numTokens; HsPtr tokens;
  do { CXSourceRange a = {{p1, p2}, d1, d2};
     CXToken * tokens;unsigned numTokens;
     clang_tokenize(t, a, &tokens, &numTokens);
      gc_result.numTokens = (HsInt)(numTokens);
      gc_result.tokens = (HsPtr)(tokens);
      
      return(&gc_result);} while(0);
}
HsInt access_prim_tokenize_numTokens(void *ptr){ return(((struct {HsInt numTokens;HsPtr tokens;}*) ptr)->numTokens);}
HsPtr access_prim_tokenize_tokens(void *ptr){ return(((struct {HsInt numTokens;HsPtr tokens;}*) ptr)->tokens);}
HsPtr prim_annotateTokens(HsPtr t,HsInt nts,HsPtr ts)
{ HsPtr cs;
  do { CXCursor * cs = (CXCursor *)malloc(sizeof(CXCursor)*nts);
     clang_annotateTokens(t, ts, nts, cs);
      
      return((HsPtr)(cs));} while(0);
}
HsPtr prim_getCursorKindSpelling(HsInt k)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getCursorKindSpelling(k);
      
      return((HsPtr)(r));} while(0);
}
HsInt prim_completionChunk_Optional()
{ HsInt res1;
  do {res1=CXCompletionChunk_Optional;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_TypedText()
{ HsInt res1;
  do {res1=CXCompletionChunk_TypedText;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Text()
{ HsInt res1;
  do {res1=CXCompletionChunk_Text;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Placeholder()
{ HsInt res1;
  do {res1=CXCompletionChunk_Placeholder;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Informative()
{ HsInt res1;
  do {res1=CXCompletionChunk_Informative;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_CurrentParameter()
{ HsInt res1;
  do {res1=CXCompletionChunk_CurrentParameter;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_LeftParen()
{ HsInt res1;
  do {res1=CXCompletionChunk_LeftParen;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_RightParen()
{ HsInt res1;
  do {res1=CXCompletionChunk_RightParen;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_LeftBracket()
{ HsInt res1;
  do {res1=CXCompletionChunk_LeftBracket;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_RightBracket()
{ HsInt res1;
  do {res1=CXCompletionChunk_RightBracket;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_LeftBrace()
{ HsInt res1;
  do {res1=CXCompletionChunk_LeftBrace;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_RightBrace()
{ HsInt res1;
  do {res1=CXCompletionChunk_RightBrace;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_LeftAngle()
{ HsInt res1;
  do {res1=CXCompletionChunk_LeftAngle;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_RightAngle()
{ HsInt res1;
  do {res1=CXCompletionChunk_RightAngle;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Comma()
{ HsInt res1;
  do {res1=CXCompletionChunk_Comma;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_ResultType()
{ HsInt res1;
  do {res1=CXCompletionChunk_ResultType;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Colon()
{ HsInt res1;
  do {res1=CXCompletionChunk_Colon;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_SemiColon()
{ HsInt res1;
  do {res1=CXCompletionChunk_SemiColon;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_Equal()
{ HsInt res1;
  do {res1=CXCompletionChunk_Equal;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_HorizontalSpace()
{ HsInt res1;
  do {res1=CXCompletionChunk_HorizontalSpace;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_completionChunk_VerticalSpace()
{ HsInt res1;
  do {res1=CXCompletionChunk_VerticalSpace;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCompletionChunkKind(HsPtr arg1,HsInt arg2)
{ HsInt res1;
  do {res1 = clang_getCompletionChunkKind(arg1, arg2);
      
      return((HsInt)(res1));} while(0);
}
HsPtr prim_getCompletionChunkText(HsPtr s,HsInt i)
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getCompletionChunkText(s, i);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getCompletionChunkCompletionString(HsPtr arg1,HsInt arg2)
{ HsPtr res1;
  do {res1 = clang_getCompletionChunkCompletionString(arg1, arg2);
      
      return((HsPtr)(res1));} while(0);
}
HsInt prim_getNumCompletionChunks(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getNumCompletionChunks(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCompletionPriority(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getCompletionPriority(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_getCompletionAvailability(HsPtr arg1)
{ HsInt res1;
  do {res1 = clang_getCompletionAvailability(arg1);
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_codeComplete_IncludeMacros()
{ HsInt res1;
  do {res1=CXCodeComplete_IncludeMacros;
      
      return((HsInt)(res1));} while(0);
}
HsInt prim_codeComplete_IncludeCodePatterns()
{ HsInt res1;
  do {res1=CXCodeComplete_IncludeCodePatterns;
      
      return((HsInt)(res1));} while(0);
}
HsPtr prim_codeCompleteAt(HsPtr t,char * s,HsInt i1,HsInt i2,HsPtr ufs,HsInt nufs,HsInt i3)
{ HsPtr r;
  do { r = clang_codeCompleteAt(t, s, i1, i2, ufs, nufs, i3);
      
      return((HsPtr)(r));} while(0);
}
void prim_sortCodeCompletionResults(HsPtr c,HsInt i)
{ do { clang_sortCodeCompletionResults(c, i);
      ;} while(0);
}
HsInt prim_codeCompleteGetNumDiagnostics(HsPtr c)
{ HsInt r;
  do { r = clang_codeCompleteGetNumDiagnostics(c);
      
      return((HsInt)(r));} while(0);
}
HsPtr prim_codeCompleteGetDiagnostic(HsPtr c,HsInt i)
{ HsPtr r;
  do { CXDiagnostic r = clang_codeCompleteGetDiagnostic(c, i);
      
      return((HsPtr)(r));} while(0);
}
HsPtr prim_getClangVersion()
{ HsPtr r;
  do { CXString *r = mkStrObj();*r = clang_getClangVersion();
      
      return((HsPtr)(r));} while(0);
}
 void prim_getInclusions_(HsPtr t, HsPtr f) { clang_getInclusions(t, f, NULL); }
