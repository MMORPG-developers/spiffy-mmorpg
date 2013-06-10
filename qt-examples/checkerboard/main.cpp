// main.cpp
// June 8, 2013
// Cecily Hunt

#include "checkerboard.hpp"

#include <QApplication>

int main(int argc, char **argv)
{
	QApplication app(argc, argv);
	Checkerboard checkerboard;

	return app.exec();
}
