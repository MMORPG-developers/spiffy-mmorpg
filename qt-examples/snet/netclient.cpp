#include "netclient.hpp"

NetClient::NetClient(QWidget *parent, Qt::WindowFlags flags)
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
    QObject::connect(input, SIGNAL(editingFinished()),
                     this, SLOT(readyToSubmit()));
    
    // Might as well put the focus in the input box.
    input->setFocus(Qt::OtherFocusReason);
}

NetClient::~NetClient()
{
    delete history;
    delete input;
    delete submit;
    
    delete sub_layout;
}

void NetClient::appendText(const QString s)
{
    history->append(s);
}

void NetClient::readyToSubmit()
{
    QString s = input->text();
    
    input->clear();
    
    emit textSubmitted(s);
}

