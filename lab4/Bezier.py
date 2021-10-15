#!/usr/bin/env python
 
"""
Bezier 曲线绘制程序

author: rewine
last edited: 2021年10月
"""
import sys,random
from PyQt5.QtWidgets import QWidget, QApplication, QSplitter, QVBoxLayout, QPushButton, QButtonGroup, QSlider, QSizePolicy
from PyQt5.QtGui import QPainter, QPainterPath, QPen, QColor
from PyQt5.QtCore import Qt, QRect, QPointF
from enum import Enum,unique
from math import sqrt
from decimal import Decimal as D

@unique
class State(Enum):
    add = 1
    move = 2
    moving = 3

def get_distance(pos1, pos2) -> float:
    return sqrt((pos1.x()-pos2.x())**2 + (pos1.y()-pos2.y())**2);

class DrawWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.initUI()
        self.data = [] # 记录点集坐标
        self.r = 10 # 圆半径（鼠标选中要在半径范围内）
        self.state = State.add
        self.selectPos = -1 # 鼠标选中的点在 data 的下标
        self.lastPos = None 
        self.globelT = D(0)

    def initUI(self):
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground)
        self.show()
    
    def setMove(self):
        self.state = State.move
    
    def setAdd(self):
        self.state = State.add

    def clearData(self):
        self.data.clear()
        self.repaint()

    def setGlobelT(self, val):
        self.globelT = val
        self.repaint()
 
    def mousePressEvent(self,  event):
        if self.state == State.add:
                    self.data.append(event.pos());
                    print(event.pos())
                    self.repaint()
        elif self.state == State.move:
            self.selectPos = -1
            for i in range(len(self.data)):
                if get_distance(self.data[i], event.pos()) < self.r:
                    self.selectPos = i
            if self.selectPos != -1:
                self.lastPos = event.pos();
                self.setMouseTracking(True)
                self.state = State.moving
    
    def mouseReleaseEvent(self, event):
        if self.state == State.moving:
            self.setMouseTracking(False)
            self.state = State.move;
            
    def mouseMoveEvent(self, event):
        if self.state == State.moving:
            print(self.lastPos)
            self.data[self.selectPos] += event.pos() - self.lastPos;
            self.lastPos = event.pos()
            self.repaint()

    def paintEvent(self, e):
        qp = QPainter()
        qp.begin(self)
        qp.setRenderHint(QPainter.Antialiasing, True); # 抗锯齿
        self.drawLines(qp)
        self.drawBezierCurve(qp)
        qp.end()

    def drawLines(self, qp):
        pen = QPen(Qt.black, 1, Qt.SolidLine) 
        qp.setPen(pen)
        for i,pos in enumerate(self.data):
            rect = QRect(pos.x()-self.r, pos.y()-self.r, self.r*2, self.r*2)
            qp.drawEllipse(rect)
            # qp.drawText(rect, Qt.AlignCenter, str(i))
            if i > 0:
                qp.drawLine(self.data[i-1], self.data[i]);
     
    def drawBezierCurve(self, qp):
        # 画出 Bezier 曲线
        if len(self.data) >= 2:
            path = QPainterPath()
            path.moveTo(self.data[0])
            step = D(0.01)
            t = D(0)
            while t <= D(1):
                bp = self.getBeziePoint(t)
                path.lineTo(QPointF(bp[-1][0][0], bp[-1][0][1]))
                t += step
            qp.drawPath(path) 

            # 画出过程点坐标
            colors = [QColor(255, 0, 0), QColor(255, 125, 0), QColor(255, 255, 0), 
                      QColor(0, 255, 0), QColor(0, 0, 255), QColor(0, 255, 255), QColor(255, 0, 255)]
            bp = self.getBeziePoint(self.globelT)
            for i in range(1, len(bp)):
                pen = QPen(colors[(i-1)%7], 5)
                qp.setPen(pen)
                for pos in bp[i]:
                    qp.drawPoint(QPointF(pos[0], pos[1]))


    def getBeziePoint(self, t):
        # 德卡斯特罗算法，获取位置比例为 t 的点
        bp = [[(D(pos.x()), D(pos.y())) for pos in self.data]]
        for i in range(len(self.data)-1):
            bp.append([])
            for j in range(len(self.data)-1-i):
                bp[-1].append(((D(1)-t)*bp[-2][j][0] + t*bp[-2][j+1][0],
                               (D(1)-t)*bp[-2][j][1] + t*bp[-2][j+1][1]));
        return bp

class MainWindow(QWidget):
    def __init__(self):
        super().__init__()
 
        self.initUI()
 
    def initUI(self):
        self.setGeometry(300, 300, 800, 500)
        self.setWindowTitle('Bezier 曲线绘制程序')

        # 右侧绘图曲
        self.rwidget = DrawWidget()

        # 左侧控制面板
        self.lwidget = QWidget()
        self.lwidget.setMaximumSize(100, 600)
        self.lvbox = QVBoxLayout()
        self.addButton = QPushButton("ADD")
        self.moveButton = QPushButton("MOVE")
        self.clearButton = QPushButton("CLEAR")
        self.slider = QSlider(Qt.Vertical)
        self.slider.setMaximumSize(100, 200)
        self.slider.setStyleSheet('''
            QSlider
            {
                background-color: rgba(22, 22, 22, 0.2);
                padding-top: 15px;  /*上面端点离顶部的距离*/
                padding-bottom: 15px;
                border-radius: 5px; /*外边框矩形倒角*/
            }
 
            QSlider::add-page:vertical
            {
               background-color: #FF7826;
                width:5px;
                border-radius: 2px;
            }
 
            QSlider::sub-page:vertical
            {
                background-color: #7A7B79;
                width:5px;
                border-radius: 2px;
            }
            QSlider::groove:vertical
            {
                background:transparent;
                width:6px;
            }
 
            QSlider::handle:vertical    
            {
                height: 14px;  
                width: 14px;
                margin: 0px -4px 0px -4px;
                border-radius: 7px;
                background: white;
            }
''')
        self.slider.setRange(0, 100)
        self.slider.setSingleStep(1)
        self.slider.setValue(0)
        
        self.addButton.clicked.connect(self.rwidget.setAdd)
        self.moveButton.clicked.connect(self.rwidget.setMove)
        self.clearButton.clicked.connect(self.rwidget.clearData)
        self.slider.valueChanged.connect(self.valuechange)

        self.lbGroup = QButtonGroup()    
        self.lvbox.addWidget(self.addButton)
        self.lvbox.addWidget(self.moveButton)
        self.lvbox.addWidget(self.clearButton)
        self.lvbox.addWidget(self.slider)
        self.lwidget.setLayout(self.lvbox)
       
        # 整体布局
        self.splitter = QSplitter(Qt.Horizontal)
        self.splitter.addWidget(self.lwidget)
        self.splitter.addWidget(self.rwidget)

        self.vbox = QVBoxLayout()
        self.vbox.addWidget(self.splitter)
        self.setLayout(self.vbox)
        self.show()
    
    def valuechange(self):
        # QSlider 值必须为整数， 0~100 映射为 0~1
        val = self.slider.value()
        print(val)
        self.rwidget.setGlobelT(D(val * 0.01))
 
 
if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())