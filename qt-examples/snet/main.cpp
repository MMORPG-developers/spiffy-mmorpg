// Snet's not exactly telnet.

#include "netclient.hpp"
#include "serverconnection.hpp"

#include <QApplication>
#include <QObject>
#include <QString>

#include <iostream>

const char *SERVER = "localhost";
const int PORT = 6667;

int main(int argc, char **argv)
{
    std::cerr << "Don't use this program." << std::endl;
    std::cerr << "By default it connects to a local spiffy server,"
        << std::endl;
    std::cerr << "but it doesn't communicate using the right protocol."
        << std::endl;
    
    return 1;
    
    
    // Never reached
    
    
    QApplication app(argc, argv);
    
    NetClient widget;
    ServerConnection connection(SERVER, PORT);
    
    QObject::connect(&widget, SIGNAL(textSubmitted(QString)),
                     &connection, SLOT(sendText(QString)));
    QObject::connect(&connection, SIGNAL(textArrived(QString)),
                     &widget, SLOT(appendText(QString)));
    
    // TODO: Add a handler for when the user closes the window.
    
    widget.show();
    
    return app.exec();
}

