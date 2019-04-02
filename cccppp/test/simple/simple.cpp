// some cases around operator[]

// 1. it's really an overload

// 2. it's really a builtin

// 3. inside a template, we could generate either
// ... ideally I want a way to generate something that will do
//       "either builtin or overload"
//         according to the context
//         ... how can I do that?

// one idea is to elaborate all the templates.
// but this can't be done at source level (at least not with Clang).
// ... so want to do some

//      template < >     maybe_builtin_brackets <>()()
//
// ... doesn't this just get us infinite regress? the same problem again?
// NO, I don't think so.
// we can have something like
//
//                   uses_builtin_if_not<  has_brackets_overload<T, ArgTypes...>    >
//
// and we can define a has_brackets_overload template based on compile-time information
// -- it defaults to false,
//     but each time we see an overload of a method, we specialize it to true


// "it's really an overload"
struct S {
	int operator[](unsigned n) const { return (int) n; }
	/* in clang the operator call comes out as
      `-CXXOperatorCallExpr 0x2f13c70 <col:37, col:46> 'int'
        |-ImplicitCastExpr 0x2f13c58 <col:44, col:46> 'int (*)(unsigned int) const' <FunctionToPointerDecay>
        | `-DeclRefExpr 0x2f13c08 <col:44, col:46> 'int (unsigned int) const' lvalue CXXMethod 0x2f13868 'operator[]' 'int (unsigned int) const'
        |-ParenExpr 0x2f13ba8 <col:37, col:43> 'const S' lvalue
        | `-UnaryOperator 0x2f13b88 <col:38, col:39> 'const S' lvalue prefix '*'
        |   `-CXXThisExpr 0x2f13b70 <col:39> 'const S *' this
        `-ImplicitCastExpr 0x2f13bf0 <col:45> 'unsigned int' <LValueToRValue>
          `-DeclRefExpr 0x2f13bc8 <col:45> 'unsigned int' lvalue ParmVar 0x2f13928 'n' 'unsigned int'
	
	 ... so it's apparent that we're doing a CXXOperatorCallExpr
	          where the ImplicitCastExpr<FunctionToPointerDecay> gives us the called lvalue
	  */
	int foo(unsigned n) const { return (*this)[n]; }
};

// it's really the builtin
struct T {
	int x;
	/* This comes out as

      `-ImplicitCastExpr 0x2f14180 <col:37, col:45> 'int' <LValueToRValue>
        `-MemberExpr 0x2f14148 <col:37, col:45> 'const int' lvalue .x 0x2f13eb8
          `-ArraySubscriptExpr 0x2f14120 <col:37, col:43> 'const T' lvalue
            |-CXXThisExpr 0x2f140c8 <col:37> 'const T *' this
            `-ImplicitCastExpr 0x2f14108 <col:42> 'unsigned int' <LValueToRValue>
              `-DeclRefExpr 0x2f140e0 <col:42> 'unsigned int' lvalue ParmVar 0x2f13f18 'n' 'unsigned int

	 ... so it's apparent that we're doing an ArraySubscriptExpr
	 */
	int foo(unsigned n) const { return this[n].x; }
};

// it could do either
template <typename X>
struct Index
{
	// case (a): depending on T, we could generate a builtin or S::operator[]
	/* This comes out as
  |-CXXMethodDecl 0x2f43ab8 <line:47:2, col:60> col:6 index_it 'int (const X &, unsigned int) const'
  | |-ParmVarDecl 0x2f438f8 <col:15, col:24> col:24 referenced x 'const X &'
  | |-ParmVarDecl 0x2f43970 <col:27, col:36> col:36 referenced n 'unsigned int'
  | `-CompoundStmt 0x2f43e18 <col:45, col:60>
  |   `-ReturnStmt 0x2f43e00 <col:47, col:57>
  |     `-ArraySubscriptExpr 0x2f43dd8 <col:54, col:57> '<dependent type>' lvalue
  |       |-DeclRefExpr 0x2f43d88 <col:54> 'const X' lvalue ParmVar 0x2f438f8 'x' 'const X &'
  |       `-DeclRefExpr 0x2f43db0 <col:56> 'unsigned int' lvalue ParmVar 0x2f43970 'n' 'unsigned int'
	
	... i.e. we get an ArraySubscriptExpr even though it actually might
	elaborate into an overload. I think we can identify this sub-case through
	the fact that the expression's type is given as "<dependent type>".
	
	IDEA: can we use SFINAE to identify the "builtin operators"
	versus
	overloaded operators
	from the fact that an "explicit operator call" will invoke the latter
	but not the former?
	YES. See scratch1.cc.
	
	
	
	 */
	int index_it(const X& x, unsigned n) const { return x[n]; }
	// case (b): it's all happening one level of indirection away -- does this affect us?
	// I think the answer is clearly no.
	int foo_it(const X& x, unsigned n) const { return x.foo(n); }
};

int f()
{
	S s[10];
	return Index<S>().foo_it(s[0], 0);
}

// --------------------------------
// What's our source-to-source rewriting of the foregoing?
#if 0
struct S {
	int x;
	int operator[](unsigned n) { return n; }
	// we can resolve the overload here, so we do nothing
	int foo(unsigned n) { return (*this)[n].x; }
};

// it's really the builtin
struct T {
	int foo(unsigned n) { return __primop_index<T, unsigned>()(*this, n); }
};

template <typename T, unsigned N>
struct Index
{
	// case (a): depending on T, we could generate a builtin or S::operator[]
	int index_it(const T& t, unsigned n) const
	{
		return __maybe_primop_index<T, unsigned, has_overload<BRACKETS, T, unsigned>::value >(t, n);
	}
	// case (b): it's all happening one level of indirection away -- does this affect us?
	// clearly no.
	int foo_it(const T& t, unsigned n) const { return t.foo(n); }
};

#endif
