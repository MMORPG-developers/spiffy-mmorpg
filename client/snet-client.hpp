#ifndef _SNET_CLIENT_HPP_INCLUDED
#define _SNET_CLIENT_HPP_INCLUDED

#include <Qt>
#include <QString>
#include <QWidget>
#include <QTextEdit>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>

class SNetClient : public QWidget {
    Q_OBJECT
    
    public:
        SNetClient(QWidget *parent=0, Qt::WindowFlags flags=0);
        ~SNetClient();
    
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

#endif // _SNET_CLIENT_HPP_INCLUDED
