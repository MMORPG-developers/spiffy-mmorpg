// main.cpp
// June 8, 2013
// Cecily Hunt

#include "clickableCheckerboard.hpp"

#include <QApplication>

int main(int argc, char **argv)
{
	QApplication app(argc, argv);
	ClickableCheckerboard clickableCheckerboard;

	return app.exec();
}
