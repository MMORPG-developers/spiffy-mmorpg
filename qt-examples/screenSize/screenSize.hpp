// screenSize.hpp
// Cecily Hunt
// July 20, 2013

// This program displays the screen count, primary screen dimensions, and
// available space on the primary screen. It's an example of how to use the
// QDesktopWidget class.

#ifndef SCREEN_SIZE_HPP_INCLUDED
#define SCREEN_SIZE_HPP_INCLUDED

#include <string>
#include <sstream>

#include <QApplication>
#include <QCoreApplication>
#include <QWidget>
#include <QLabel>
#include <QGridLayout>
#include <QDesktopWidget>

using namespace std;

class ScreenSize : public QWidget
{
    Q_OBJECT

    public:
        ScreenSize(QWidget *parent = 0);
        ~ScreenSize();

    private:
        void setText();
        QString formatDimensions(QRect size);

        // This should really be in its own class for style purposes, but I am
        // too lazy to make that happen.
        string toString(int input);

        QLabel *screenCountLabel_;
        QLabel *screenCount_;
        QLabel *dimensionsLabel_;
        QLabel *dimensions_;
        QLabel *availableDimensionsLabel_;
        QLabel *availableDimensions_;
        QGridLayout *layout_;
        QDesktopWidget *desktop_;
};

#endif // SCREEN_SIZE_HPP_INCLUDED
