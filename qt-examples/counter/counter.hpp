// counter.hpp
// June 8, 2013
// Cecily Hunt

// Declarations for the counter class. The widget created has a button labeled
// click, a number, and a button labeled quit. The number shows how many times
// the click button has been clicked.

#ifndef _COUNTER_HPP_INCLUDED
#define _COUNTER_HPP_INCLUDED

#include <QWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QLabel>

class Counter : public QWidget
{
	Q_OBJECT

	public:
		Counter(QWidget *parent = 0);
		~Counter();

	private slots:
		void showCount();
		void quitClicked();

	private:
		QLabel *countDisplay; 	// the text display field
		QPushButton *count; 	// the "click" button
		QPushButton *quit; 		// the "quit" button
		QVBoxLayout *layout; 	// the widget's layout
        int counter; 			// an int storing the current count
};

#endif // _COUNTER_HPP_INCLUDED
