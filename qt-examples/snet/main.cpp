// Snet's not exactly telnet.

#include "netclient.hpp"
#include "serverconnection.hpp"

#include <QApplication>
#include <QObject>
#include <QString>

const char *SERVER = "localhost";
const int PORT = 6667;

int main(int argc, char **argv)
{
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

