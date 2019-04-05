int main(int argc, char **argv)
{
	return static_cast<int>(reinterpret_cast<unsigned long>(argv[0]));
}
