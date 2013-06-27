/*
 * See comment in snet-client.hpp.
 */

#include "snet-client.hpp"

SNetClient::SNetClient(QWidget *parent, Qt::WindowFlags flags)
    : QWidget(parent, flags)
{
    history = new QTextEdit(this);
    input = new QLineEdit(this);
    submit = new QPushButton("Send", this);
    
    history->setReadOnly(true);
    
    layout = new QGridLayout(this);
    
    layout->addWidget(history, 0, 0, 1, 2);
    layout->addWidget(input, 1, 0);
    layout->addWidget(submit, 1, 1);
    
    setLayout(layout);
    
    QObject::connect(submit, SIGNAL(clicked()), this, SLOT(readyToSubmit()));
    QObject::connect(input, SIGNAL(returnPressed()),
                     this, SLOT(readyToSubmit()));
    
    // Might as well put the focus in the input box.
    input->setFocus(Qt::OtherFocusReason);
}

SNetClient::~SNetClient()
{
    delete history;
    delete input;
    delete submit;
    
    delete layout;
}

void SNetClient::appendText(QString s)
{
    history->append(s);
}

void SNetClient::readyToSubmit()
{
    QString s = input->text();
    
    input->clear();
    
    emit textSubmitted(s);
}

