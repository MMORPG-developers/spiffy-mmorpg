// checkerboard.hpp

#ifndef _CHECKERBOARD_HPP_INCLUDED
#define _CHECKERBOARD_HPP_INCLUDED

#include <QGraphicsView>
#include <QGraphicsScene>

class Checkerboard : public QGraphicsScene
{
	Q_OBJECT

	public:
		Checkerboard(QWidget *parent = 0);
		~Checkerboard();

	private:
		QGraphicsView *view;
		const static int SIDE_LENGTH = 50;
};

#endif // _CHECKERBOARD_HPP_INCLUDED
