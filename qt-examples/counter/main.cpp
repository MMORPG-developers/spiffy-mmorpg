// main.cpp
// June 8, 2013
// Cecily Hunt

// Runs the counter widget described in counter.hpp.

#include "counter.hpp"

#include <QApplication>

int main(int argc, char **argv)
{
	QApplication app(argc, argv);
	Counter counter;

	return app.exec();
}
