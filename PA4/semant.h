#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <symtab.h>
#include <map>
#include "cool-tree.h"
#include "stringtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

template <class Elem> class InheritGraph;

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  InheritGraph<Class_>* inheritGraph;
  // globalType contains all classes type
  SymbolTable<Symbol, Class__class>* globalType;
  // globalMethod contains all methods of all classes
  std::map<Class_, SymbolTable<Symbol, method_class>*> classMethod;
  // classAttr contains all attr for each class
  std::map<Class_, SymbolTable<Symbol, attr_class>*> classAttr;
  // when error ocurrerd and need to halt, set this as false
  bool move_on;


public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  bool should_move_on() { return move_on; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Class_ c, Expression e);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

