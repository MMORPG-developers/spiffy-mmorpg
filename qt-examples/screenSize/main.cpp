// main.cpp
// June 8, 2013
// Cecily Hunt

#include <QApplication>

#include "screenSize.hpp"

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    ScreenSize screensize;

	return app.exec();
}
