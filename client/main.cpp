#include <iostream>
#include <string>

#include "network.hpp"

using namespace std;

const char *SERVER = "192.168.2.2";
// The port is specified as a string containing its decimal representation, not
// as the number itself. Presumably because the port needs to be sent as a
// string over the network anyway, but still... seriously guys?
const char *PORT = "6667";

void run()
{
    Socket sock(SERVER, PORT);
    sock.send("\0\0\0\4", 4); // 4 bytes
    sock.send("asdf", 4); // name
    sock.send("\0\0\0\1", 4); // 1 byte
    sock.send("\1", 1); // request map
    sock.receive(1); // here's a map
    sock.receive(4); // rows
    sock.receive(4); // columns
    string response = sock.receive(4096); // the map
    
    cout << "Received \"\"\""
         << response
         << "\"\"\" from server."
         << endl;
    
    cout << "Yay, server/client communications work!" << endl;
}

int main(void)
{
    try {
        run();
    }
    catch (exception *err) {
        cerr << err->what() << endl;
    }
    
    return 0;
}

