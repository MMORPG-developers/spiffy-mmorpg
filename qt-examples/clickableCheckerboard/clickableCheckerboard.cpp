// clickableCheckerboard.cpp
// June 8, 2013
// Cecily Hunt

#include "clickableCheckerboard.hpp"

ClickableCheckerboard::ClickableCheckerboard(QWidget *parent)
{
	squares = new QGraphicsWidget*[NUM_SQUARES];
	for (int i = 0; i != NUM_SQUARES; i++) {
		squares[i] = new QGraphicsWidget[NUM_SQUARES];
	}

	for (int i = 0; i != NUM_SQUARES; i++) {
		for (int j = 0; j != NUM_SQUARES; j++) {
			int x = i * SIDE_LENGTH;
			int y = j * SIDE_LENGTH;
			squares[i][j].setGeometry(x, y, SIDE_LENGTH, SIDE_LENGTH);
		}
	}

	view = new QGraphicsView(this);

	view->show();
}

ClickableCheckerboard::~ClickableCheckerboard()
{
	for (int i = 0; i != NUM_SQUARES; i++) {
		delete [] squares[i];
	}
	delete [] squares;
	delete view;
}
