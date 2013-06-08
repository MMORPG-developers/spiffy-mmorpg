// counter.cpp
// June 8, 2013
// Cecily Hunt

// Definitions for the counter class.

#include "counter.hpp"

#include <QCoreApplication>

Counter::Counter(QWidget *parent)
	: QWidget(parent)
{
	// Initialize private data members
	countDisplay = new QLabel(this);
	count = new QPushButton("Click", this);
	quit = new QPushButton("Quit", this);
	layout = new QVBoxLayout;
	counter = 0;

	// Add widgets to the layout
	layout->addWidget(count);
	layout->addWidget(countDisplay);
	layout->addWidget(quit);

	// Create the layout
	setLayout(layout);

	// Show the initial count before we show the app window. If this happens
	// after we call show(), then there is no value displayed initially.
	showCount();

	// Show the app window.
	show();

	// Set up the connections between different pieces of the app. These
	// connections tell the app to call certain functions when certain events
	// occur. Look up doc on signals and slots if this is confusing.
	QObject::connect(count, SIGNAL(clicked()), this, SLOT(showCount()));
	QObject::connect(quit, SIGNAL(clicked()), this, SLOT(quitClicked()));
}

Counter::~Counter()
{
	delete count;
	delete countDisplay;
	delete quit;
	delete layout;
}

void Counter::showCount()
{
	countDisplay->setNum(counter);
	counter++;
}

void Counter::quitClicked()
{
	QCoreApplication::quit();
}
