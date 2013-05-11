#ifndef _NETCLIENT_HPP_INCLUDED
#define _NETCLIENT_HPP_INCLUDED

#include <Qt>
#include <QString>
#include <QWidget>
#include <QTextEdit>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>

class NetClient : public QWidget {
    Q_OBJECT
    
    public:
        NetClient(QWidget *parent=0, Qt::WindowFlags flags=0);
        ~NetClient();
    
    signals:
        void textSubmitted(QString s);
    
    public slots:
        void appendText(QString s);
    
    private slots:
        void readyToSubmit();
    
    private:
        QTextEdit *history;
        QLineEdit *input;
        QPushButton *submit;
        
        QVBoxLayout *main_layout;
        QHBoxLayout *sub_layout;
};

#endif // _NETCLIENT_HPP_INCLUDED
