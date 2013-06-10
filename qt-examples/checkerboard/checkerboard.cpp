// checkerboard.cpp
// June 8, 2013
// Cecily Hunt

#include "checkerboard.hpp"

#include <QPen>
#include <QBrush>

Checkerboard::Checkerboard(QWidget *parent)
{
	QBrush blue = Qt::darkRed;
	QBrush green = Qt::darkCyan;
	for (int i = 0; i != 10; i++) {
		for (int j = 0; j != 10; j++) {
			int x = i * SIDE_LENGTH;
			int y = j * SIDE_LENGTH;
			if ((i + j) % 2 == 0) {
				addRect(x, y, SIDE_LENGTH, SIDE_LENGTH, Qt::NoPen, blue);
			} else {
				addRect(x, y, SIDE_LENGTH, SIDE_LENGTH, Qt::NoPen, green);
			}
		}
	}

	view = new QGraphicsView(this);

	view->show();
}

Checkerboard::~Checkerboard()
{
	delete view;
}
