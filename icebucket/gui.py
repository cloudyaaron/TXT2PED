#!/usr/bin/python

import sys

from PyQt5.QtGui import QPixmap
from pip._internal import main
import subprocess
import platform

# from PyQt5 import QtCore
try:
    from PyQt5.QtWidgets import *
except ModuleNotFoundError:
    print("PYQT5 missing, installing......")
    if __name__ == '__main__':
        main(['install', 'PyQt5'])
    print('relaunch after PyQt5 installed')
    input('press any key to exit')
    exit()


# window for user to choose file
class Window(QWidget):

    def __init__(self):
        super(Window, self).__init__()
        self.resize(1366, 768)
        self.ss = QCheckBox(self)
        self.textline = QTextEdit(self)
        self.textline.resize(300,500)
        self.console = QTextBrowser(self)
        self.graphview = QLabel(self)
        self.graphview.resize(600, 600)
        self.graphview.move(850, 0)
        self.ss.setText('Strong suggest (testing)')
        self.ss.move(10, 600)
        self.g = QGraphicsView()
        self.filename = './src/sample.txt'

        self.console.resize(500, 500)
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
        self.confirm.setEnabled(True)
        f = open(self.filename, 'r', encoding='utf-8')
        ft = f.read()
        self.textline.setText(ft)
        f.close()

    def open_dic(self):
        file, file_type = QFileDialog.getOpenFileName(self, "open", "./", "TEXT FILES (*.txt)")  #

        self.filename = file
        print(file_type)
        if file_type != '':
            self.confirm.setEnabled(True)
        self.filename = file
        f = open(file, 'r', encoding='utf-8')
        ft = f.read()
        self.textline.setText(ft)
        f.close()

    # show user what's in the file
    def preview(self):
        self.confirm.setEnabled(True)

        text = self.textline.toPlainText()
        f = open(self.filename, 'w', encoding='utf-8')
        f.write(text)
        # pwd = self.textline.toPlainText()
        # pwd = pwd.replace("/", "\\")
        f.close()
        cmd = 'Rscript PedigreeEngine.R ' + self.filename
        if self.ss.isChecked():
            print('ss enable')
            cmd = cmd + ' -s'
        try:
            feedback = subprocess.check_output(cmd, shell=True)
            text = feedback.decode('utf-8')
            pic = QPixmap('output.jpg')
            self.graphview.setPixmap(pic)
        except:
            f = open('output.ped', 'r', encoding='utf-8')
            text = f.read()
            text = text + '\n Error occur'
            f.close()
        self.console.setText(text)


# Welcome menu need more polish and ui friendly design
class StartWindow(QWidget):
    def __init__(self):
        super(StartWindow, self).__init__()
        self.resize(1024, 768)
        self.ui = Window()
        self.setWindowTitle('BINF6112 Pedigree Engine')
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
