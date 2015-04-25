<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. 特点</a></li>
<li><a href="#sec-2">2. 安装</a></li>
<li><a href="#sec-3">3. 设置</a>
<ul>
<li><a href="#sec-3-1">3.1. 设置英文字体</a></li>
<li><a href="#sec-3-2">3.2. 设置中文字体</a></li>
<li><a href="#sec-3-3">3.3. 设置中英文字体等宽</a></li>
<li><a href="#sec-3-4">3.4. 换行设置</a></li>
</ul>
</li>
</ul>
</div>
</div>

# 特点<a id="sec-1" name="sec-1"></a>

chinese设置层整合了ELPA中和中文写作有关的包，目的是为使用spacemacs进行中文写作的同学提供帮助，节省大家的时间。其中包含了以下和中文写作有关的包：
-   **ace-pinyin:** 根据汉字的拼音首字母进行ace-jump。
-   **chinese-pyim:** 目前最好用的emacs拼音输入法，带tooltip。
-   **find-by-pinyin-dired:** 根据拼音首字母找到并打开文件。
-   **pangu-spacing:** 以“软空格”的形式在中英混排文档中自动增加汉字与英文单词的间距。
-   **pinyin-search:** 顾名思义，根据汉字拼音首字母进行查找。
-   **visual-fill-column:** 使visual-line-mode按照fill-column的值wrap。
-   **vlf（可选）:** 使emacs可以轻松处理大文件，便于编辑输入法词库文件（大多在10MB以上）。

# 安装<a id="sec-2" name="sec-2"></a>

安装非常简单，首先

    git clone https://github.com/et2010/Chinese.git ~/.emacs.d/private

然后在你的.spacemacs文件中添加chinese设置层。

# 设置<a id="sec-3" name="sec-3"></a>

## 设置英文字体<a id="sec-3-1" name="sec-3-1"></a>

首先，利用spacemacs自身提供的函数设置英文字体：

    dotspacemacs-default-font '("Source Code Pro"
                                :size 14
                                :weight normal
                                :width normal
                                :powerline-scale 1.2)

Spacemacs默认的字体为“Source Code Pro”，默认字号为13. 为了方便进行下边的设置，我将字号修改为14.（当然，这是试出来的。）

**上述设置可以在spacemacs的配置文件（.spacemacs）中找到。**

## 设置中文字体<a id="sec-3-2" name="sec-3-2"></a>

在dotspacemacs/config函数中添加下面这段代码，用于设置中文字体。这里我用了“新宋体”（NSimSun），可以替换为系统中已安装的任意字体。

    (when window-system
      ;; "CJK Unified Ideographs" (han) U+4E00 - U+9FFF
      (set-fontset-font "fontset-default"
                        (cons (decode-char 'ucs #x4e00)
                              (decode-char 'ucs #x9fff))
                        "NSimSun"))

**请将这一段代码插入到dotspacemacs/config函数中去。**

## 设置中英文字体等宽<a id="sec-3-3" name="sec-3-3"></a>

另外，当使用GUI的emacs时，默认的中文字符宽度一般不等于英文字符宽度的两倍，这样会导致用字符画的表格非常丑。解决办法是分别设置中文和英文字体的缩放倍数，使中文字体的宽度刚好等于英文字符宽度的两倍：

    (setq face-font-rescale-alist
          '(
            (".*Source Code Pro.*" . 1.0)
            (".*NSimSun.*" . 1.15)
            ))

这里我通过试验发现英文字号为14 （字号用 `C-u C-x =` 查看），中文字号为16刚好可以实现等宽。因此将英文字体字号直接设为14, 放大倍数设为1.0 （即不变）。中文字体默认字号为13, 放大1.15倍即得到16号（ **注意你的电脑上设置可能会不同** ）。英文字体不放大，只放大中文字体――这样设置是最优的，较少发生行高跳动等glitch。

**请将这一段代码插入到dotspacemacs/config函数中去。**

## 换行设置<a id="sec-3-4" name="sec-3-4"></a>

本layer使用visual-line-mode进行软换行（即不在行尾插入回车）。但visual-line只能在window边缘换行，不够美观。所以加入了visual-fill-column，实现在fill-column处换行。
