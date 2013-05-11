#include "netclient.hpp"

#include <QApplication>

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    
    NetClient widget;
    widget.show();
    
    return app.exec();
}

