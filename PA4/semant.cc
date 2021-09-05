

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <vector>
#include <iostream>
#include <set>
#include <map>
#include "stringtab.h"
#include "semant.h"
#include "utilities.h"

//////////////////////////////////////////////////////////////////////
// Implement the extends of cool-tree
//////////////////////////////////////////////////////////////////////

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

static attr_class* self_attr;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");

	self_attr	= new attr_class(self, SELF_TYPE, new no_expr_class());
}
//////////////////////////////////////////////////////////////////////
// a simple graph data structure with a simple iterator
//////////////////////////////////////////////////////////////////////
struct Info;
template <class Elem> class InheritGraph;
template <class Elem> class NodeIterator;

struct Info
{
    bool inherits_from;
};

//////////////////////////////////////////////////////////////////////
//  NodeIterator
//   A simple iterator for class InheritGraph. 
//   
//   The public methods are:
//       operator ++()
//         Move to the next node. Will move even if reached the end
//       operator --()
//         Move to previous node. Won't move if reached the start
//       has_more()
//         return 1 if haven't reached the end
//         return 0 if reached the end
//       size()
//         return the sum of all elems the iterator can go over
//       operator *()
//         return the Elem elem itself
//////////////////////////////////////////////////////////////////////
template <class Elem> class NodeIterator
{
public:
    NodeIterator<Elem> operator ++ ();
    NodeIterator<Elem> operator -- ();
    int has_more();
    int size();
    NodeIterator(std::vector<Info> all_children, const std::vector<Elem>* all_nodes); // child iterator
    NodeIterator(const std::vector<Elem>* all_nodes); // all nodes iterator
    Elem operator *();
private:
    int current_idx;
    std::vector<Elem> all_nodes;
};

template<class Elem>
int NodeIterator<Elem>::size()
{
    return all_nodes.size();
}

template<class Elem>
Elem NodeIterator<Elem>::operator*()
{
    return all_nodes[current_idx];
}

template<class Elem>
NodeIterator<Elem> NodeIterator<Elem>::operator++()
{
    current_idx++;
    return *this;
}

template<class Elem>
NodeIterator<Elem> NodeIterator<Elem>::operator--()
{
    if (current_idx > 0)
		current_idx--;
    return *this;
}

template<class Elem>
int NodeIterator<Elem>::has_more()
{
    return current_idx < (int)all_nodes.size();
}

template<class Elem>
NodeIterator<Elem>::NodeIterator(std::vector<Info> all_children, const std::vector<Elem>* all_nodes)
{
    current_idx = 0;

    for (int i = 0; i < (int)all_children.size(); i++)
    {
        if (all_children[i].inherits_from)
        {
            this->all_nodes.push_back((*all_nodes)[i]);
        }
    }
}

template<class Elem>
NodeIterator<Elem>::NodeIterator(const std::vector<Elem>* all_nodes)
{
    current_idx = 0;

    this->all_nodes = (*all_nodes);
}

//////////////////////////////////////////////////////////////////////
//  InheritGraph
//   A simple graph structure
//  
//  The public method
//   void add_relation(Elem parent, Elem child)
//     add a inherit relationship between parent and child
//   Elem* get_parent(Elem son)
//     return a pointer point to the direct ancestor of son
//     return NULL if son have no ancestor
//   NodeIterator<Elem> get_childern_iter(Elem parent);
//     return a iterator of all childern of parent
//   NodeIterator<Elem> get_all_nodes_iter();
//     return a iterator of all classes
//   get_depth(Elem node)
//     return a int of node's inherit depth in the graph
//////////////////////////////////////////////////////////////////////

template <class Elem> class InheritGraph
{
public:
    void add_relation(Elem parent, Elem child);
    Elem* get_parent(Elem son);
    NodeIterator<Elem> get_childern_iter(Elem parent);
    NodeIterator<Elem> get_all_nodes_iter();
	int get_depth(Elem node);

    InheritGraph<Elem>(): current_node_idx(0), built_depth_flag(false) {}

private:
    int elem2idx(Elem elem);
	void build_depth();
	void build_depth_dfs(Elem node, int depth);
	bool built_depth_flag;

    std::vector< std::vector<Info> > graph_map;
    std::vector<int> direct_parent_idx;
    std::vector<int> inherit_depth;
    std::vector<Elem> all_nodes;

    std::map<Elem, int> elem2idx_map;
    int current_node_idx;
};

template<class Elem>
void InheritGraph<Elem>::build_depth_dfs(Elem node, int depth)
{
	auto iter = get_childern_iter(node);
	for (; iter.has_more(); ++iter)
	{
		inherit_depth[elem2idx(*iter)] = depth + 1;
		build_depth_dfs(*iter, depth + 1);
	}
}

template<class Elem>
void InheritGraph<Elem>::build_depth()
{
	for (int i = 0; i < current_node_idx; i++)
	{
		if (inherit_depth[i] == 0)
		{
			build_depth_dfs(all_nodes[i], 0);
		}
	}
	built_depth_flag = true;
}

template<class Elem>
int InheritGraph<Elem>::get_depth(Elem node)
{
	if (!built_depth_flag) build_depth();
	return inherit_depth[elem2idx(node)];
}

template<class Elem>
Elem* InheritGraph<Elem>::get_parent(Elem son)
{
    if (direct_parent_idx[elem2idx(son)] != -1)
        return &all_nodes[direct_parent_idx[elem2idx(son)]];
    else
        return NULL;
}

template<class Elem>
NodeIterator<Elem> InheritGraph<Elem>::get_childern_iter(Elem parent)
{
    NodeIterator<Elem> iter(graph_map[elem2idx(parent)], &all_nodes);
    return iter;
}

template<class Elem>
NodeIterator<Elem> InheritGraph<Elem>::get_all_nodes_iter()
{
    NodeIterator<Elem> iter(&all_nodes);
    return iter;
}

template<class Elem>
void InheritGraph<Elem>::add_relation(Elem parent, Elem child)
{
    int parent_idx = elem2idx(parent);
    int child_idx = elem2idx(child);
    int max_idx = std::max(parent_idx, child_idx);

    if (max_idx > (int)graph_map.size())
    {
        direct_parent_idx.resize(max_idx + 100, -1);
		inherit_depth.resize(max_idx + 100, 0);
        graph_map.resize(max_idx + 100);
        /*
		unsupported by g++ 4.4.3
		for (auto &i : graph_map)
        {
            i.resize(max_idx + 100);
        }
		*/
		for (unsigned int i = 0; i < graph_map.size(); i++)
		{
			graph_map[i].resize(max_idx + 100);
		}
    }

    graph_map[parent_idx][child_idx].inherits_from = 1;
    direct_parent_idx[child_idx] = parent_idx;
	inherit_depth[child_idx] = 1;
}

template<class Elem>
int InheritGraph<Elem>::elem2idx(Elem elem)
{
    auto elem_idx_pair = elem2idx_map.find(elem);
    if (elem_idx_pair == elem2idx_map.end())
    {
        // element haven't added
        elem2idx_map[elem] = current_node_idx;
        current_node_idx++;
        all_nodes.push_back(elem);
    }
	return elem2idx_map[elem];
}

//////////////////////////////////////////////////////////////////////
//  template<class Elem> static void has_acyclic(InheritGraph<Elem> graph)
//
//   A function test whether a InheritGraph has a inherit acyclic
//   report error and return true if there do have a acyclic
//   return false if the graph is good
//////////////////////////////////////////////////////////////////////
template<class Elem>
static bool has_acyclic_dfs(InheritGraph<Elem>& graph, 
                            const Elem& parent, 
                            std::set<Elem>& visited,
							ClassTable& error)
{
	bool error_flag = false;

    if (visited.find(parent) != visited.end())
    {
        // has a acyclic inherits error
        Elem acyclic_class = parent;
        Elem parent_of_acyclic_class = (*graph.get_parent(parent));
        do
        {
			// report error here, acyclic_class is involved in the acyclic
			error.semant_error(acyclic_class) << "Class " <<
			acyclic_class->get_name()->get_string() <<
			", or an ancestor of " <<
			acyclic_class->get_name()->get_string() <<
			", is involved in an inheritance cycle." << std::endl;
            acyclic_class = parent_of_acyclic_class;
            parent_of_acyclic_class = (*(graph.get_parent(parent_of_acyclic_class)));
        } while (acyclic_class != parent);
        return true;
    }

    visited.insert(parent);
    for (auto iter = graph.get_childern_iter(parent); iter.has_more(); ++iter)
    {
        if (has_acyclic_dfs(graph, (*iter), visited, error))
		{
			error_flag = true;
		}
    }

	return error_flag;
}

template<class Elem>
static bool has_acyclic(InheritGraph<Elem>& graph, ClassTable& error)
{
	bool error_flag = false;
    auto iter = graph.get_all_nodes_iter();
    std::set<Elem> visited;

    for (; iter.has_more(); ++iter)
    {
        if (visited.find((*iter)) == visited.end() && graph.get_parent((*iter)) != NULL)
		{
			if (has_acyclic_dfs(graph, (*iter), visited, error) == true)
				error_flag = true;
		}
    }
	return error_flag;
}

static bool build_typeTable(
	const Classes& classes, 
	SymbolTable<Symbol, Class__class>& globalType,
	ClassTable& error)
{
	Class_ current_class;
	bool error_flag = false;
	bool found_Main_class_flag = false;
	for (int i = classes->first(); classes->more(i); i = classes->next(i))
	{
		current_class = classes->nth(i);
		if ( strcmp(current_class->get_name()->get_string(), "Main") == 0)
		{
			found_Main_class_flag = true;
		}
		if (globalType.probe(current_class->get_name()) != NULL)
		{
			// a redefine error ocrrued
			error.semant_error(current_class) << "Class " << 
			current_class->get_name()->get_string() << 
			" was previously defined." << std::endl;
			error_flag = true;
		}
		else
		{
			globalType.addid(current_class->get_name(), current_class);
		}
	}
	if (!found_Main_class_flag)
	{
		error.semant_error() << "Class Main is not defined." << std::endl;
	}
	return error_flag;
}

static bool build_inherit_graph(
	const Classes& classes,
	SymbolTable<Symbol, Class__class>& globalType,
	InheritGraph<Class_>& inheritGraph,
	ClassTable& error)
{
	Class_ current_class;
	Class_ parent_of_current_class;
	bool error_flag = false;
	for (int i = classes->first(); classes->more(i); i = classes->next(i))
	{
		current_class = classes->nth(i);
		parent_of_current_class = globalType.probe(current_class->get_parent());
		if (parent_of_current_class == NULL)
		{
			// error: inherits from a undefined class
			error.semant_error(current_class) << "Class " <<
			current_class->get_name()->get_string() <<
			" inherits from an undefined class " <<
			current_class->get_parent()->get_string() << std::endl;
			error_flag = true;
		}
		else
		{
			inheritGraph.add_relation(parent_of_current_class, current_class);
		}

	}
	#undef DEBUG
	#ifdef DEBUG
		inheritGraph.show_all_relation();
	#endif
	#define DEBUG
	return error_flag;
}

static bool build_method_and_attr_table_for_one_class(
	Class_ current_class,
    SymbolTable<Symbol, Class__class>& globalType,
	std::map<Class_, SymbolTable<Symbol, method_class>*>& classMethod,
	std::map<Class_, SymbolTable<Symbol, attr_class>*>& classAttr,
	ClassTable& error)
{
	Features features;
	Feature feature;
	method_class* method;
	attr_class* attr;
	// error handle
	bool error_flag = false;
	bool main_method_found_flag = false;
	Class_ main_class = NULL;

	if (strcmp(current_class->get_name()->get_string(), "Main") == 0) { main_class = current_class; }

	// add table for each class
	classAttr[current_class]->enterscope();
	classAttr[current_class]->addid(self_attr->get_name(), self_attr);
	classMethod[current_class]->enterscope();

	features = current_class->get_features();
	for (int j = features->first(); features->more(j); j = features->next(j))
	{
		feature = features->nth(j);
		if (feature->get_feature_type() == ATTR_SEMANT)
		{
			// attr
			attr = (attr_class*) feature;
			if (classAttr[current_class]->probe(attr->get_name()) != NULL)
			{
				// attr redefined error
				error.semant_error(current_class) << "Attribute " <<
				attr->get_name()->get_string() << " is multiply defined in class " <<
				current_class->get_name()->get_string() << std::endl;	
				error_flag = true;
			}
			else
			{
				classAttr[current_class]->addid(attr->get_name(), attr);
			}
		}
		else
		{
			// method
			method = (method_class*) feature;
			if (classMethod[current_class]->probe(method->get_name()) != NULL)
			{
				// method redefined error
				error.semant_error(current_class) << "Method " <<
				method->get_name()->get_string() << " is multiply defined in class " <<
				current_class->get_name()->get_string() << std::endl;	
				error_flag = true;
			}
			else
			{
				if ((strcmp(current_class->get_name()->get_string(), "Main") == 0) &&
					(strcmp(method->get_name()->get_string(), "main") == 0) )
				{
					main_method_found_flag = true;
				}
				classMethod[current_class]->addid(method->get_name(), method);
			}
		}
	}

	if (!main_method_found_flag && main_class != NULL)
	{
		// main not defined error
		error.semant_error(main_class) << "No 'main' method in class Main." << std::endl;
	} 
	return error_flag;
}

static bool build_method_and_attr_table_dfs(
	Class_ parent,
    SymbolTable<Symbol, Class__class>& globalType,
	InheritGraph<Class_>& inheritGraph, 
	std::map<Class_, SymbolTable<Symbol, method_class>*>& classMethod,
	std::map<Class_, SymbolTable<Symbol, attr_class>*>& classAttr,
	ClassTable& error)
{
	bool error_flag = false;
	
	if (build_method_and_attr_table_for_one_class(parent, globalType, classMethod, classAttr, error))
	{
		error_flag = true;
	}
	for (auto iter = inheritGraph.get_childern_iter(parent); iter.has_more(); ++iter)
	{
		classAttr[*iter] = new SymbolTable<Symbol, attr_class>(*classAttr[parent]);
		classMethod[*iter] = new SymbolTable<Symbol, method_class>(*classMethod[parent]);
		if (build_method_and_attr_table_dfs(*iter, globalType, inheritGraph, classMethod, classAttr, error))
		{
			error_flag = true;
		}
	}
	return error_flag;
}

static bool build_method_and_attr_table(
    SymbolTable<Symbol, Class__class>& globalType,
	InheritGraph<Class_>& inheritGraph, 
	std::map<Class_, SymbolTable<Symbol, method_class>*>& classMethod,
	std::map<Class_, SymbolTable<Symbol, attr_class>*>& classAttr,
	ClassTable& error)
{
	Class_ Object_class = globalType.lookup(Object);
	classAttr[Object_class] = new SymbolTable<Symbol, attr_class>();
	classMethod[Object_class] = new SymbolTable<Symbol, method_class>();
	return build_method_and_attr_table_dfs(Object_class, globalType, inheritGraph, classMethod, classAttr, error);
}

// least upper bound of two classes
static Class_ lub(Class_ c1, Class_ c2, InheritGraph<Class_>& inheritGraph)
{
	if (c1 == NULL && c2 == NULL) return NULL;
	if (c1 == NULL) return c2;
	if (c2 == NULL) return c1;
	int depth1 = inheritGraph.get_depth(c1);
	int depth2 = inheritGraph.get_depth(c2);
	while (depth1 < depth2)
	{
		c2 = *(inheritGraph.get_parent(c2));
		depth2 = inheritGraph.get_depth(c2);
	}
	while (depth1 > depth2)
	{
		c1 = *(inheritGraph.get_parent(c1));
		depth1 = inheritGraph.get_depth(c1);
	}
	while (c1 != c2)
	{
		// we can asure they can always get a parent and lca is always exist
		c1 = *(inheritGraph.get_parent(c1));
		c2 = *(inheritGraph.get_parent(c2));
	}
	return c1;
}

static Symbol expr_typing(
	Expression expr,
	Class_& current_class, 
	Classes& classes, 
	InheritGraph<Class_>& inheritGraph,
	SymbolTable<Symbol, Class__class>& globalType, 
	SymbolTable<Symbol, Entry>& localSymbolType,
	std::map<Class_, SymbolTable<Symbol, method_class>*>& classMethod,
	std::map<Class_, SymbolTable<Symbol, attr_class>*>& classAttr,
	ClassTable& error)
{
	Expressions body;
	Symbol return_type1;
	Symbol return_type2;
	Symbol return_type3;
	Symbol name1;
	Symbol name2;
	Symbol class_name;
	Symbol formal_name;
	Symbol formal_type;
	Symbol dispatcher_type;
	Formal formal;
	Formals formals;
	Formal dispatch_formal;
	Expression dispatch_actual;
	Expressions dispatch_actuals;
	method_class* method;
	attr_class* attr1 = NULL;
	char* error_operation;
	Class_ class1 = NULL;
	Class_ class2 = NULL;
	Case branch;
	Cases cases;
	int formal_idx;
	int actual_idx;

	switch (expr->get_expr_type())
	{
	case ASSIGN_SEMANT :
		name1 = expr->get_first_symbol();
		// first, lookup in the localSymbolType
		return_type1 = localSymbolType.lookup(name1);
		if (return_type1 == NULL)
		{
			// didn't found in localSymbolType, then lookup in the classAttr
			attr1 = (attr_class*)classAttr[current_class]->lookup(name1);
			if (attr1 == NULL)
			{
				// can't found, meaning an error
				error.semant_error(current_class, expr) << "Undeclared identifier " << 
				name1->get_string() << "." << std::endl;
				return_type1 = Object;
			}
			else
			{
				return_type1 = attr1->get_second_symbol();
			}
		}
		return_type2 = expr_typing(expr->get_first_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != return_type2 && return_type2 != No_type)	
		{
			// error
			error.semant_error(current_class, expr) << "Type " <<
			return_type2->get_string() << " of assigned expression does not conform to declared type " << 
			return_type1->get_string() << " of identifier " << 
			name1->get_string() << "." << std::endl;
		}
		expr->set_type(return_type1);
		break;         
	case STATIC_DISPATCH_SEMANT :
		dispatcher_type = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		class_name = expr->get_first_symbol();
		name1 = expr->get_second_symbol();
		class1 = globalType.lookup(class_name);
		if (class1 == NULL)
		{
			// error unknown class
			error.semant_error(current_class, expr) << "Static dispatch to undefined class " << 
			class_name->get_string() << "." << std::endl;
			expr->set_type(Object);
		}
		else
		{
			method = classMethod[class1]->lookup(name1);
			if (method == NULL)
			{
				// error unknown method
				error.semant_error(current_class, expr) << "Static dispatch to undefined method " << 
				name1->get_string() << "." << std::endl;
				expr->set_type(Object);
			}
			else
			{
				// first check all the formals
				formals = method->get_formals();	
				formal_idx = formals->first(); 
				dispatch_actuals = expr->get_expr_list();
				actual_idx = dispatch_actuals->first();
				while (1)
				{
					if (formals->more(formal_idx) != dispatch_actuals->more(actual_idx))
					{
						// num of formal conflict
						error.semant_error(current_class, expr) << "Method " << 
						method->get_name()->get_string() << " invoked with wrong number of arguments." << std::endl;
						break;
					}
					else if (formals->more(formal_idx) == 0)
					{
						// end of formal, there is no error!
						break;
					}

					formal = formals->nth(formal_idx);
					dispatch_actual = dispatch_actuals->nth(actual_idx);
					return_type1 = formal->get_second_symbol();
					return_type2 = expr_typing(dispatch_actual, current_class, 
					classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
					class1 = globalType.lookup(return_type1); // formal class
					class2 = globalType.lookup(return_type2); // acutal class
					if (class1 == NULL)
					{
						// undefined type
						class1 = globalType.lookup(Object);
					}
					else if (lub(class1, class2, inheritGraph) != class2)
					{
						// type conflict
						error.semant_error(current_class, expr) << "In call of method " << 
						method->get_name()->get_string() << ", type " << return_type2->get_string() << 
						" of parameter " << formal->get_first_symbol()->get_string() << " does not conform to declared type " << return_type1->get_string() << std::endl;
					}

					formal_idx = formals->next(formal_idx);
					actual_idx = dispatch_actuals->next(actual_idx);
				}
				// lastly process the return type
				return_type3 = method->get_return_type();
				if (return_type3 == SELF_TYPE)
				{
					return_type3 = dispatcher_type;
				}
				expr->set_type(return_type3);
			}
		}
		break;
	case DISPATCH_SEMANT :
		dispatcher_type = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		name1 = expr->get_first_symbol();
		if (dispatcher_type == SELF_TYPE) class1 = current_class;
		else class1 = globalType.lookup(dispatcher_type);
		method = classMethod[class1]->lookup(name1);
		if (method == NULL)
		{
			// error unknown method
			error.semant_error(current_class, expr) << "Dispatch to undefined method " << 
			name1->get_string() << "." << std::endl;
			expr->set_type(Object);
		}
		else
		{
			// first check all the formals
			formals = method->get_formals();	
			formal_idx = formals->first(); 
			dispatch_actuals = expr->get_expr_list();
			actual_idx = dispatch_actuals->first();
			while (1)
			{
				if (formals->more(formal_idx) != dispatch_actuals->more(actual_idx))
				{
					// num of formal conflict
					error.semant_error(current_class, expr) << "Method " << 
					method->get_name()->get_string() << " invoked with wrong number of arguments." << std::endl;
					break;
				}
				else if (formals->more(formal_idx) == 0)
				{
					// end of formal, there is no error!
					break;
				}

				formal = formals->nth(formal_idx);
				dispatch_actual = dispatch_actuals->nth(actual_idx);
				return_type1 = formal->get_second_symbol();
				return_type2 = expr_typing(dispatch_actual, current_class, 
				classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
				class1 = globalType.lookup(return_type1); // formal class
				class2 = globalType.lookup(return_type2); // acutal class
				if (class1 == NULL)
				{
					// undefined type
					class1 = globalType.lookup(Object);
				}
				else if (lub(class1, class2, inheritGraph) != class2)
				{
					// type conflict
					error.semant_error(current_class, expr) << "In call of method " << 
					method->get_name()->get_string() << ", type " << return_type2->get_string() << 
					" of parameter " << formal->get_first_symbol()->get_string() << " does not conform to declared type " << return_type1->get_string() << std::endl;
				}

				formal_idx = formals->next(formal_idx);
				actual_idx = dispatch_actuals->next(actual_idx);
			}
			// lastly process the return type
			return_type3 = method->get_return_type();
			if (return_type3 == SELF_TYPE)
			{
				return_type3 = dispatcher_type;
			}
			expr->set_type(return_type3);
		}
		break;
	case COND_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type2 = expr_typing(expr->get_second_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type3 = expr_typing(expr->get_third_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != Bool)
		{
			error.semant_error(current_class, expr) << "Predicate of 'if' does not have type Bool." << std::endl;
		}
		class1 = globalType.lookup(return_type2);
		class2 = globalType.lookup(return_type3);
		return_type1 = lub(class1, class2, inheritGraph)->get_name();
		expr->set_type(return_type1);
		break;           
	case LOOP_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type2 = expr_typing(expr->get_second_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != Bool)
		{
			error.semant_error(current_class, expr) << "Loop condition does not have type Bool." << std::endl;
		}
		expr->set_type(Object);
		break;           
	case TYPECASE_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		cases = expr->get_cases();
		for (int i = cases->first(); cases->more(i); i = cases->next(i))
		{
			branch = cases->nth(i);
			localSymbolType.enterscope();
			localSymbolType.addid(expr->get_first_symbol(), expr->get_second_symbol());
			class2 = globalType.lookup(expr_typing(branch->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error));
			localSymbolType.exitscope();
			class1 = lub(class1, class2, inheritGraph);
		}
		expr->set_type(class1->get_name());
		break;        
	case BLOCK_SEMANT :
		body = expr->get_expr_list();
		for (int i = body->first(); body->more(i); i = body->next(i))
		{
			return_type1 = expr_typing(body->nth(i), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		}
		expr->set_type(return_type1);
		break;
	case LET_SEMANT :
		name1 = expr->get_first_symbol();
		return_type1 = expr->get_second_symbol(); // type of name1
		if (globalType.lookup(return_type1) == NULL && return_type1 != SELF_TYPE)
		{
			// error of undefined type
			error.semant_error(current_class, expr) << "Class " << return_type1->get_string() <<
			" of let-bound identifier " << name1->get_string() << " is undefined." << std::endl;
		}
		return_type2 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != return_type2 && return_type2 != No_type)
		{
			// error
			error.semant_error(current_class, expr) << "Inferred type " <<
			return_type2->get_string() << " of initialization of " << 
			name1->get_string() << " does not conform to identifier's declared type " << 
			return_type1->get_string() << "." << std::endl;
		}
		localSymbolType.enterscope();
		localSymbolType.addid(name1, return_type1);
		return_type3 = expr_typing(expr->get_second_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		localSymbolType.exitscope();
		expr->set_type(return_type3);
		break;           
	case PLUS_SEMANT :
	case SUB_SEMANT :
	case MUL_SEMANT :
	case DIVIDE_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type2 = expr_typing(expr->get_second_expr(), current_class,
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		// compare this two, if they are both Int, return the type
		// if not, report error and set return type as Int
		if ((return_type1 != return_type2) || (return_type1 != Int))
		{
			// error
			switch (expr->get_expr_type())
			{
				case PLUS_SEMANT : error_operation = " + "; break;
				case SUB_SEMANT : error_operation = " - "; break;
				case MUL_SEMANT : error_operation = " * "; break;
				case DIVIDE_SEMANT : error_operation = " / "; break;
			}
			error.semant_error(current_class, expr) << "non-Int arguments: " << 
			return_type1->get_string() << error_operation << return_type2->get_string() << std::endl;
		}
		expr->set_type(Int);
		break;
	case NEG_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != Int)
		{
			error.semant_error(current_class, expr) << 
			"Argument of '~' has type " << 
			return_type1->get_string() << " instead of Int." << std::endl;
		}
		// always return Int type here even an error occured
		// because the `~` only retrun Int 
		expr->set_type(Int);
		break;
	case LT_SEMANT :
	case LEQ_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type2 = expr_typing(expr->get_second_expr(), current_class,
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		// compare this two, if they are both Int, return the type
		// if not, report error and set return type as Bool 
		if ((return_type1 != return_type2) || (return_type1 != Int))
		{
			// error
			switch (expr->get_expr_type())
			{
				case LT_SEMANT : error_operation = " < "; break;
				case LEQ_SEMANT : error_operation = " <= "; break;
			}
			error.semant_error(current_class, expr) << "non-Int arguments: " << 
			return_type1->get_string() << error_operation << return_type2->get_string() << std::endl;
		}
		expr->set_type(Bool);
		break;
	case EQ_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		return_type2 = expr_typing(expr->get_second_expr(), current_class,
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		// If either <expr1> or <expr2> has static type 
		// Int, Bool, or String, then the other must have 
		// the same static type. Any other types, including
		// SELF TYPE, may be freely compared. 
		// if not, report error and set return type as Bool 
		if ( ((return_type1 == Int || return_type1 == Str || return_type1 == Bool) ||
			  (return_type2 == Int || return_type2 == Str || return_type2 == Bool)) )
		{
			// one of them are basic type
			if (return_type1 != return_type2)
			{
				// error
				error.semant_error(current_class, expr) << "Illegal comparison with a basic type." << std::endl;
			}
		}
		expr->set_type(Bool);
		break;          
	case COMP_SEMANT :
		return_type1 = expr_typing(expr->get_first_expr(), current_class, 
			classes, inheritGraph, globalType, localSymbolType, classMethod, classAttr, error);
		if (return_type1 != Bool)
		{
			error.semant_error(current_class, expr) << 
			"Argument of 'not' has type " << 
			return_type1->get_string() << " instead of Int." << std::endl;
		}
		// always return Int type here even an error occured
		// because the `~` only retrun Int 
		expr->set_type(Bool);
		break;
	case INT_CONST_SEMANT :
		expr->set_type(Int);
		break;
	case BOOL_CONST_SEMANT :
		expr->set_type(Bool);
		break;
	case STRING_CONST_SEMANT :
		expr->set_type(Str);
		break;
	case NEW__SEMANT :
		return_type1 = expr->get_first_symbol();
		if (return_type1 != SELF_TYPE && globalType.lookup(return_type1) == NULL)
		{
			// error
			error.semant_error(current_class, expr) << "'new' used with undefined class " <<
			return_type1->get_string() << "." << std::endl;
			expr->set_type(Object);
		}
		else
		{
			expr->set_type(return_type1);
		}
		break;
	case ISVOID_SEMANT :
		expr_typing(expr->get_first_expr(), current_class, 
		classes, inheritGraph, globalType, localSymbolType, 
		classMethod, classAttr, error);
		expr->set_type(Bool);
		break;
	case NO_EXPR_SEMANT :
		expr->set_type(No_type);
		break;
	case OBJECT_SEMANT :
		name1 = expr->get_first_symbol();
		// first, lookup in the localSymbolType
		return_type1 = localSymbolType.lookup(name1);
		if (return_type1 == NULL)
		{
			// didn't found in localSymbolType, then lookup in the classAttr
			attr1 = (attr_class*)classAttr[current_class]->lookup(name1);
			if (attr1 == NULL)
			{
				// can't found, meaning an error
				error.semant_error(current_class, expr) << "Undeclared identifier " << 
				name1->get_string() << "." << std::endl;
				return_type1 = Object;
			}
			else
			{
				return_type1 = attr1->get_second_symbol();
			}
		}
		expr->set_type(return_type1);
		break;
	default :
		// will never reach here
		fatal_error("unknow expr");
		exit(-1);
	}
	return expr->get_type();
}


static void type_checking_and_inferring(
	Classes& classes, 
	InheritGraph<Class_>& inheritGraph,
	SymbolTable<Symbol, Class__class>& globalType,
	std::map<Class_, SymbolTable<Symbol, method_class>*>& classMethod,
	std::map<Class_, SymbolTable<Symbol, attr_class>*>& classAttr,
	ClassTable& error)
{
	Class_ current_class;
	Features features;
	Feature feature;
	method_class* method;
	attr_class* attr;
	Symbol return_type;
	Symbol method_return_type;
	Symbol attr_type;
	Symbol formal_name;
	Symbol formal_type;
	Formal formal;
	Formals formals;
	SymbolTable<Symbol, Entry> *localSymbolType = new SymbolTable<Symbol, Entry>();

	for (int i = classes->first(); classes->more(i); i = classes->next(i))
	{
		current_class = classes->nth(i);
		features = current_class->get_features();
		for (int j = features->first(); features->more(j); j = features->next(j))
		{
			feature = features->nth(j);
			if (feature->get_feature_type() == ATTR_SEMANT)
			{
				// attr
				// handle the init
				attr = (attr_class*) feature;
				attr_type = attr->get_second_symbol();
				if (attr->get_first_expr()->get_expr_type())
				{
					return_type = expr_typing(attr->get_first_expr(), current_class, classes, 
					inheritGraph, globalType, *localSymbolType, classMethod, classAttr, error);
					if (attr_type != return_type && return_type != No_type)
					{
						error.semant_error(current_class, attr->get_first_expr()) << 
						"Inferred type " << return_type->get_string() << " of initialization of attribute " <<
						attr->get_name()->get_string() << " does not conform to declared type " <<
						attr_type->get_string() << "." << std::endl;
					}
				}
			}
			else
			{
				// method
				method = (method_class*) feature;
				// first handle all the formals
				localSymbolType->enterscope();
				formals = method->get_formals();
				if (!strcmp("Main", current_class->get_name()->get_string()))
				{
					if (!strcmp("main", method->get_name()->get_string()))
					{
						if (formals->more(formals->first()))
						{
							error.semant_error(current_class->get_filename(), method) << "'main' method in class Main should have no arguments." << std::endl;
						}
					}
				}
				else
				{
					for (int i = formals->first(); formals->more(i); i = formals->next(i))
					{
						formal = formals->nth(i);
						formal_name = formal->get_first_symbol();
						formal_type = formal->get_second_symbol();
						if (formal_type == SELF_TYPE)
						{
							// SELF_TYPE can't use in formal
							error.semant_error(current_class->get_filename(), formal) << "Formal parameter a cannot have type SELF_TYPE." << std::endl;
							formal_type = Object;
						}
						else if (globalType.lookup(formal_type) == NULL)
						{
							// undefined type
							error.semant_error(current_class->get_filename(), formal) << "Class " << formal_type->get_string() << 
							" of formal parameter " << formal_name->get_string() << " is undefined." << std::endl;
						}
						localSymbolType->addid(formal_name, formal_type);
					}
				}
				// second handle the expr
				return_type = expr_typing(method->get_first_expr(), current_class, classes, 
				inheritGraph, globalType, *localSymbolType, classMethod, classAttr, error);
				method_return_type = method->get_return_type();
				Class_ class1, class2;
				if (return_type == SELF_TYPE)
				{
					class1 = current_class;
				}
				else
				{
					class1 = globalType.lookup(return_type);
				}
				if (method_return_type == SELF_TYPE)
				{
					class2 = current_class;
				}
				else
				{
					class2 = globalType.lookup(method_return_type);
				}
				if (lub(class1, class2, inheritGraph)->get_name() == Object)
				{
					error.semant_error(current_class->get_filename(), method) << "Inferred return type " << 
					return_type->get_string() << " of method " << method->get_name()->get_string() << " does not conform to declared return type " <<
					method_return_type->get_string() << "." << std::endl;
				}
			}
		}
	}


}

ClassTable::ClassTable(Classes classes) 
	: semant_errors(0) , error_stream(cerr) 
{
	move_on = true;
	inheritGraph = new InheritGraph<Class_>();
	globalType = new SymbolTable<Symbol, Class__class>();

	// 0. add basic classes and setup globalType
	install_basic_classes();
	// 1. pass one: fill globalType 
	if (build_typeTable(classes, *globalType, *this)) { move_on = false; return; }
	// 2. pass two: build inheritGraph
	if (build_inherit_graph(classes, *globalType, *inheritGraph, *this)) { move_on = false; return; }
	// 3. check if there is a inherit acyclic 
	if (has_acyclic(*inheritGraph, *this)) { move_on = false; return; }
	// 4. pass three: build classMethod and classAttr 
	if (build_method_and_attr_table(*globalType, *inheritGraph, classMethod, classAttr, *this)) { move_on = false; return; }
	// 5. pass four: typing
	type_checking_and_inferring(classes, *inheritGraph, *globalType, classMethod, classAttr, *this);
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

	globalType->enterscope();
		globalType->addid(Object_class->get_name(), Object_class);
		globalType->addid(IO_class->get_name(), IO_class);
		globalType->addid(Int_class->get_name(), Int_class);
		globalType->addid(Bool_class->get_name(), Bool_class);
		globalType->addid(Str_class->get_name(), Str_class);
	inheritGraph->add_relation(Object_class, IO_class);
	inheritGraph->add_relation(Object_class, Int_class);
	inheritGraph->add_relation(Object_class, Bool_class);
	inheritGraph->add_relation(Object_class, Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Class_ c, Expression e)
//       print line number and filename for `e' of `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////
ostream& ClassTable::semant_error(Class_ c, Expression e)
{
    return semant_error(c->get_filename(),e);
}

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


