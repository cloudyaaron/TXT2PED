#!/usr/bin/python

import sys
import pip
import subprocess

# from PyQt5 import QtCore
try:
    from PyQt5.QtWidgets import *
except ModuleNotFoundError:
    print("PYQT5 missing, installing......")
    if __name__ == '__main__':
        pip.main(['install', 'pyqt5'])
    print('relaunch after PYQT5 installed')
    input('press any key to exit')
    exit()


# window for user to choose file
class Window(QWidget):

    def __init__(self):
        super(Window, self).__init__()
        self.resize(1024, 768)
        self.textline = QTextBrowser(self)
        self.console = QTextBrowser(self)
        self.console.resize(500, 600)
        self.console.move(300, 0)
        self.button = QPushButton(self)
        self.confirm = QPushButton(self)
        self.confirm.setText("read file")
        self.button.setToolTip("Print directory path in the console")
        self.button.setText("Open directory")
        self.confirm.clicked.connect(self.preview)
        self.button.clicked.connect(self.open_dic)
        self.button.move(512, 600)
        self.confirm.move(412, 600)
        self.setWindowTitle('BINF6112')
        self.confirm.setEnabled(False)

    def open_dic(self):
        file, file_type = QFileDialog.getOpenFileName(self, "open", "./", "TEXT FILES (*.txt)")  #

        print(file_type)
        if file_type != '':
            self.confirm.setEnabled(True)
        self.textline.setText(file)

    # show user what's in the file
    def preview(self):
        self.confirm.setEnabled(False)

        pwd = self.textline.toPlainText()
        # pwd = pwd.replace("/", "\\")
        f = open(pwd, encoding='utf-8')

        print(f)
        print(pwd)
        ft = f.read()
        self.textline.setText(ft)
        f.close()
        cmd = 'Rscript.exe PedigreeEngine.R ' + pwd
        feedback = subprocess.check_output(cmd, shell=True)
        self.console.setText(feedback.decode('UTF-8'))


# Welcome menu need more polish and ui friendly design
class StartWindow(QWidget):
    def __init__(self):
        super(StartWindow, self).__init__()
        self.resize(1024, 768)
        self.ui = Window()
        self.setWindowTitle('BINF6112')
        self.start_button = QPushButton(self)
        self.start_button.setText("start choose file")
        self.start_button.move(400, 384)
        self.start_button.clicked.connect(self.open_window)

    def open_window(self):
        self.ui.show()
        self.hide()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    # ui = Window()
    start = StartWindow()
    start.show()
    # ui.show()

    sys.exit(app.exec())
