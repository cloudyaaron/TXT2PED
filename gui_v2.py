import sys

import pip._internal as ppin
import subprocess
import os
import platform

# from PyQt5 import QtCore
try:
    from PyQt5.QtWidgets import *
    from PyQt5.QtGui import QPixmap
    from PyQt5.uic import loadUi
except ModuleNotFoundError:
    print("PyQt5 missing, installing......")
    if __name__ == '__main__':
        ppin.main(['install', 'PyQt5'])
    print('relaunch after PyQt5 installed')
    input('press any key to exit')
    exit()


class GuiWindow(QWidget):
    def __init__(self):
        super(GuiWindow, self).__init__()
        loadUi('./src/gui.ui', self)
        blanked = QPixmap('./src/blank.png')
        self.graphv.setPixmap(blanked)
        self.filename = './src/sample.txt'
        f = open(self.filename, 'r', encoding='utf-8')
        ft = f.read()
        self.inputfield.setText(ft)
        f.close()

        self.choosefilebutton.clicked.connect(self.open_dic)
        self.generatebutton.clicked.connect(self.preview)
        self.newfilebutton.clicked.connect(self.new_file)



    def open_dic(self):
        file, file_type = QFileDialog.getOpenFileName(self, "open", "./", "TEXT FILES (*.txt)")  #

        self.filename = file
        print(file_type)
        if file == '':
            return 1
        self.filename = file
        f = open(file, 'r', encoding='utf-8')
        ft = f.read()
        self.inputfield.setText(ft)
        f.close()
        self.refresh()

    # show user what's in the file
    def preview(self):
        text = self.inputfield.toPlainText()
        if text == '':
            return 1
        f = open(self.filename, 'w', encoding='utf-8')
        f.write(text)
        f.close()
        cmd = 'Rscript PedigreeEngine.R ' + self.filename
        if self.sscheckbox.isChecked():
            print('ss enable')
            cmd = cmd + ' -s'
        f = open('./output/log.txt', 'r', encoding='utf-8')
        text = f.read()
        f.close()
        try:
            feedback = subprocess.check_output(cmd, shell=True)
            text = feedback.decode('utf-8')
            pic = QPixmap('./output/output.jpg')
            self.graphv.setPixmap(pic)
        except subprocess.CalledProcessError as e:

            text = text + '\n Error occur\n' + e.output.decode()
            pic = QPixmap('./src/blank.png')
            self.graphv.setPixmap(pic)
        self.console.setText(text)
        self.refresh()

    # open a newfile and auto save current file
    def new_file(self):

        text = self.inputfield.toPlainText()
        f = open(self.filename, 'w+', encoding='utf-8')
        f.write(text)
        f.close()
        index = 1
        newfile = 'ped_input_' + str(index) + '.txt'
        while os.path.isfile(newfile) == True:
            index += 1
            newfile = './ped_input_' + str(index) + '.txt'
        f = open(newfile, 'w+', encoding='utf-8')
        text = f.read()
        f.close()
        self.filename = newfile
        self.inputfield.setText(text)
        self.console.setText(text)
        alert = QMessageBox()
        alert.setIcon(QMessageBox.Warning)
        alert.setText('Old file auto saved, new file has been created.\n new file name: '+newfile)
        alert.setWindowTitle('New file created')
        alert.setStandardButtons(QMessageBox.Ok)
        pic = QPixmap('./src/blank.png')
        self.graphv.setPixmap(pic)
        alert.exec_()
        self.refresh()

    def refresh(self):
        self.console.repaint()
        self.graphv.repaint()
        self.inputfield.repaint()
        self.console.moveCursor(self.console.textCursor().End)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    start = GuiWindow()
    start.show()
    sys.exit(app.exec())
