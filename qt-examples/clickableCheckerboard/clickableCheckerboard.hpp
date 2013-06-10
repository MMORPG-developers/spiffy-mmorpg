// grid.hpp

#ifndef _CLICKABLE_CHECKERBOARD_HPP_INCLUDED
#define _CLICKABLE_CHECKERBOARD_HPP_INCLUDED

#include <QGraphicsScene>
#include <QGraphicsWidget>
#include <QGraphicsView>

class ClickableCheckerboard : public QGraphicsScene
{
	Q_OBJECT

	public:
		ClickableCheckerboard(QWidget *parent = 0);
		~ClickableCheckerboard();

	private:
		QGraphicsWidget **squares;
		QGraphicsView *view;
		const static int SIDE_LENGTH = 50;
		const static int NUM_SQUARES = 10;
};

#endif // _CLICKABLE_CHECKERBOARD_HPP_INCLUDED
