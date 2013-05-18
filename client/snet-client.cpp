#include "snet-client.hpp"

SNetClient::SNetClient(QWidget *parent, Qt::WindowFlags flags)
    : QWidget(parent, flags)
{
    history = new QTextEdit(this);
    input = new QLineEdit(this);
    submit = new QPushButton("Send", this);
    
    history->setReadOnly(true);
    
    main_layout = new QVBoxLayout(this);
    sub_layout = new QHBoxLayout(this);
    
    sub_layout->addWidget(input);
    sub_layout->addWidget(submit);
    
    main_layout->addWidget(history);
    main_layout->addLayout(sub_layout);
    
    setLayout(main_layout);
    
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
    
    delete sub_layout;
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

