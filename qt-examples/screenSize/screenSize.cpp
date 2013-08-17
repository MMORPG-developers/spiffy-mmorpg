// screenSize.cpp
// Cecily Hunt
// July 20, 2013

#include "screenSize.hpp"

using namespace std;

ScreenSize::ScreenSize(QWidget *parent)
    : QWidget(parent)
{
    screenCountLabel_ = new QLabel(this);
    screenCount_ = new QLabel(this);
    dimensionsLabel_ = new QLabel(this);
    dimensions_ = new QLabel(this);
    availableDimensionsLabel_ = new QLabel(this);
    availableDimensions_ = new QLabel(this);
    layout_ = new QGridLayout;
    desktop_ = QApplication::desktop();

    // Add the labels to the grid.
    layout_->addWidget(screenCountLabel_, 0, 0);
    layout_->addWidget(screenCount_, 0, 1);
    layout_->addWidget(dimensionsLabel_, 1, 0);
    layout_->addWidget(dimensions_, 1, 1);
    layout_->addWidget(availableDimensionsLabel_, 2, 0);
    layout_->addWidget(availableDimensions_, 2, 1);

    setLayout(layout_);

    setText();

    show();
}

ScreenSize::~ScreenSize()
{
    delete screenCountLabel_;
    delete screenCount_;
    delete dimensionsLabel_;
    delete dimensions_;
    delete availableDimensionsLabel_;
    delete availableDimensions_;
    delete layout_;
}

void ScreenSize::setText()
{
    QString screenCountLabel = "Screen Count:";
    screenCountLabel_->setText(screenCountLabel);

    screenCount_->setNum(desktop_->screenCount());

    QString dimensionsLabel = "Primary Screen Dimensions:";
    dimensionsLabel_->setText(dimensionsLabel);

    QRect screenSize = desktop_->screenGeometry();
    dimensions_->setText(formatDimensions(screenSize));

    QString availableDimensionsLabel = "Primary Screen Available Dimensions:";
    availableDimensionsLabel_->setText(availableDimensionsLabel);

    QRect availableSize = desktop_->availableGeometry();
    availableDimensions_->setText(formatDimensions(availableSize));


}

QString ScreenSize::formatDimensions(QRect size)
{
    int height = size.height();
    int width = size.width();
    string sizeStr = toString(width) + " x " + toString(height);
    QString dimensions = sizeStr.c_str();
    return dimensions;
}

string ScreenSize::toString(int input)
{
    stringstream ss;
    ss << input;
    string str = ss.str();
    return str;
}
