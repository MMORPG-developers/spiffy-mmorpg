#include <Qt>
// #include <QObject>
#include <QApplication>
#include <QPixmap>
#include <QLabel>
#include <QGridLayout>

const char *TL_IMAGE_FILE = "images/lena.jpg";
const char *TR_IMAGE_FILE = "images/wide.png";
const char *BL_IMAGE_FILE = "images/text.svg";
const char *BR_IMAGE_FILE = "images/xkcd.png";

const int IMAGE_WIDTH = 256;
const int IMAGE_HEIGHT = 256;

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    
    // Widget that will be the window.
    QFrame frame;
    
    // Load the images into QPixmaps.
    QPixmap tl_image(TL_IMAGE_FILE);
    QPixmap tr_image(TR_IMAGE_FILE);
    QPixmap bl_image(BL_IMAGE_FILE);
    QPixmap br_image(BR_IMAGE_FILE);
    
    // Rescale them all to the same size.
    // In the case of the wide one (tr_image), preserve the aspect ratio
    // (Qt::KeepAspectRatio).
    tl_image = tl_image.scaled(IMAGE_WIDTH, IMAGE_HEIGHT);
    tr_image = tr_image.scaled(IMAGE_WIDTH, IMAGE_HEIGHT, Qt::KeepAspectRatio);
    bl_image = bl_image.scaled(IMAGE_WIDTH, IMAGE_HEIGHT);
    br_image = br_image.scaled(IMAGE_WIDTH, IMAGE_HEIGHT);
    
    // QPixmaps aren't widgets, so wrap them in QLabels.
    QLabel tl_widget(&frame);
    QLabel tr_widget(&frame);
    QLabel bl_widget(&frame);
    QLabel br_widget(&frame);
    
    tl_widget.setPixmap(tl_image);
    tr_widget.setPixmap(tr_image);
    bl_widget.setPixmap(bl_image);
    br_widget.setPixmap(br_image);
    
    // Let some of them scale with their QLabels.
    // It looks like if we want to have them scale but preserve their aspect
    // ratio, we'll need to write code to call the scale function ourselves.
    // So lets not do that. For now, don't let the wide one resize.
    tl_widget.setScaledContents(true);
 // tr_widget.setScaledContents(true);
    bl_widget.setScaledContents(true);
    br_widget.setScaledContents(true);
    
    // Put them all in a layout manager so they can be displayed in a grid.
    QGridLayout layout(&frame);
    
    layout.addWidget(&tl_widget, 0, 0);
    layout.addWidget(&tr_widget, 0, 1);
    layout.addWidget(&bl_widget, 1, 0);
    layout.addWidget(&br_widget, 1, 1);
    
    frame.setLayout(&layout);
    
    // Actually show the window.
    frame.show();
    
    return app.exec();
}

