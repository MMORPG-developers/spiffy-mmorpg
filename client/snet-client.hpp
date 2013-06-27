/*
 * This file is currently not used. As such it hasn't been properly commented
 * and probably isn't the best code. However, it's being kept around because we
 * may want to reproduce it later for debugging (maybe have a flag to the
 * client that puts it in debugging/snet mode?), and this way we don't have to
 * go digging around in the repository for it when that happens.
 */

#ifndef _SNET_CLIENT_HPP_INCLUDED
#define _SNET_CLIENT_HPP_INCLUDED

#include <Qt>
#include <QString>
#include <QWidget>
#include <QTextEdit>
#include <QLineEdit>
#include <QPushButton>
#include <QGridLayout>

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
        
        QGridLayout *layout;
};

#endif // _SNET_CLIENT_HPP_INCLUDED
