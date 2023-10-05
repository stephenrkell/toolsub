// prelude for cccppp
// NOTE: this is a .hpp file but it is never preprocessed! it is simply prepended
// to the output of cccppp-tool.

template <typename Subscripted>
struct __primop_subscript {};

// HACK: do some test instrumentation
extern "C" {
void warnx(const char *fmt, ...);
}

// borrowed from stack overflow! + modified
// https://stackoverflow.com/questions/5839357/detect-operator-support-with-decltype-sfinae
template <class T>
struct __has_subscript_overload
{
    // a lambda whose return type is the type of constructing a single char
    // *after* a comma operator that evaluates the "<" operator on type U
    template <class U>
    static auto subscript_test(const U* u) -> decltype(operator[]((*u), 0), char(0)) {}
    // a static member function that returns a length-2 thing
    static short subscript_test(...) {return 0;}
    static const bool value = (sizeof(subscript_test((T*)0)) == 1);
};

template <typename Element, unsigned ArrSz>
struct __primop_subscript<Element[ArrSz]>
{
	Element& operator()(
		Element (&arr)[ArrSz],
		int idx)
	{
		warnx("Indexing an array (length %u) at %p by %d", ArrSz, arr, idx);
		return arr[idx];
	}
};

template <typename PtrTarget>
struct __primop_subscript<PtrTarget *>
{
	PtrTarget& operator()(
		PtrTarget *p,
		int idx)
	{
		warnx("Indexing a pointer %p by %d", p, idx);
		return p[idx];
	}
};

template <typename Subscripted, bool Really>
struct __maybe_primop_subscript
{
	auto operator()(Subscripted s, int idx) -> decltype(operator[](*(Subscripted *)0, 0))
	{ return operator[](s, idx); }
};

template <typename Subscripted>
struct __maybe_primop_subscript<Subscripted, true>
 : __primop_subscript<Subscripted> {};
