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
<li><a href="#sec-3-5">3.5. 词库文件</a></li>
</ul>
</li>
</ul>
</div>
</div>

# 特点<a id="sec-1" name="sec-1"></a>

本设置层整合了ELPA中和中文写作有关的包，目的是为使用spacemacs进行中文写作的同学提供帮助，节省大家的时间。其中包含了以下和中文写作有关的包：

-   **ace-pinyin:** 根据汉字的拼音首字母进行ace-jump。
-   **cdlatex:** 方便在org中输入latex公式。
-   **chinese-pyim:** 目前最好用的emacs拼音输入法，带tooltip。
-   **find-by-pinyin-dired:** 根据拼音首字母找到并打开文件。
-   **pangu-spacing:** 以“软空格”的形式在中英混排文档中自动增加汉字与英文单词的间距。
-   **pinyin-search:** 顾名思义，根据汉字拼音首字母进行查找。
-   **visual-fill-column:** 使visual-line-mode按照fill-column的值wrap。
-   **vlf（可选）:** 使emacs可以轻松处理大文件，便于编辑输入法词库文件（大多在10MB以上）。

# 安装<a id="sec-2" name="sec-2"></a>

安装非常简单，首先

    git clone https://github.com/et2010/Chinese.git ~/.emacs.d/private

然后在你的.spacemacs文件中添加chinese设置层。配置类型选择emacs，这是因为本设置层主要针对emacs型配置进行优化，并且在进行行内输入时，emacs比vim的insert state更好用。

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

在dotspacemacs/config函数中添加下面这段代码，用于设置中文字体。这里我用了"Adobe Song Std"，字号（高度）为16px，和可以替换为系统中已安装的任意字体。

    (when window-system
      ;; "CJK Unified Ideographs" (han) U+4E00 - U+9FFF
      (set-fontset-font "fontset-default"
                        (cons (decode-char 'ucs #x4e00)
                              (decode-char 'ucs #x9fff))
                        "-*-Adobe Song Std-*-*-*-*-16-*-*-*-*-*-*-*")

**请将这一段代码插入到dotspacemacs/config函数中去。**

## 设置中英文字体等宽<a id="sec-3-3" name="sec-3-3"></a>

另外，当使用GUI的emacs时，默认的中文字符宽度一般不等于英文字符宽度的两倍，这样会导致用字符画的表格非常丑。解决办法是分别采用不同字号（高度）的中英文字体，使中文字符的宽度刚好等于英文字符宽度的两倍。通过试验发现不少常用的英文等宽字体字号设为14，中文字号设为16时可以满足宽度要求。注意这里使用的是绝对字号，不是放大系数。且中英文字符高度都以像素为单位，因而不论你的屏幕ppi是多少，这样设置都可以满足中英文字体等宽。使用上述字号，以下表格中的中英字体任意组合都可以满足等宽：

<table> 


<colgroup>
<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">中文字体(16)</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">**Adobe Fangsong Std**</td>
</tr>


<tr>
<td class="left">Adobe Heiti Std</td>
</tr>


<tr>
<td class="left">Adobe Kaiti Std</td>
</tr>


<tr>
<td class="left">**Adobe Song Std**</td>
</tr>


<tr>
<td class="left">**NSimSun**</td>
</tr>


<tr>
<td class="left">WenQuanYi Micro Hei Mono</td>
</tr>


<tr>
<td class="left">WenQuanYi Zen Hei Mono</td>
</tr>


<tr>
<td class="left">**微软雅黑**</td>
</tr>
</tbody>
</table>

<table>


<colgroup>
<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">英文字体(14)</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">**Anonymous Pro**</td>
</tr>


<tr>
<td class="left">**Bitstream Vera Sans Mono**</td>
</tr>


<tr>
<td class="left">BPmono</td>
</tr>


<tr>
<td class="left">**Consolas**</td>
</tr>


<tr>
<td class="left">Courier 10 Pitch</td>
</tr>


<tr>
<td class="left">Courier New (serif)</td>
</tr>


<tr>
<td class="left">Cousine</td>
</tr>


<tr>
<td class="left">**DejaVu Sans Mono**</td>
</tr>


<tr>
<td class="left">Droid Sans Mono</td>
</tr>


<tr>
<td class="left">**Envy Code R**</td>
</tr>


<tr>
<td class="left">FreeMono (serif)</td>
</tr>


<tr>
<td class="left">Liberation Mono</td>
</tr>


<tr>
<td class="left">**Monaco**</td>
</tr>


<tr>
<td class="left">MonospaceTypewriter</td>
</tr>


<tr>
<td class="left">Nimbus Mono L (serif)</td>
</tr>


<tr>
<td class="left">NotCourierSans</td>
</tr>


<tr>
<td class="left">**Oxygen Mono**</td>
</tr>


<tr>
<td class="left">**Source Code Pro**</td>
</tr>


<tr>
<td class="left">TlwgMono (serif)</td>
</tr>
</tbody>
</table>

<table>


<colgroup>
<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">英文字体(16)</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">Audimat Mono</td>
</tr>


<tr>
<td class="left">**Inconsolata**</td>
</tr>


<tr>
<td class="left">monofur</td>
</tr>


<tr>
<td class="left">**Ubuntu Mono**</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">英文字体(12)</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">Linux Libertine Mono O (serif)</td>
</tr>
</tbody>
</table>

**注意：不同系统中显示的字体族名不同，比如同一个字体，在Linux中显示为"Adobe Song Std"，在Windows中显示为"Adobe 宋体 Std L"。以上结果是在不同的操作系统上试验得到的，你遇到的字体名与上面这些可能会略有差异。**

除了中文16号，英文14号这种组合之外，从表中可以看到有少量英文字体设为16号也可以和16号的中文字体等宽，即同时实现等高和等宽。此外，“Linux Libertine Mono O”这种字体字号为12时可以实现与16号中文字体等宽（但不推荐使用，因为与中文字体高度相差较大）。推荐使用表中加粗的字体。

## 换行设置<a id="sec-3-4" name="sec-3-4"></a>

本layer使用visual-line-mode实现软换行（除了为了分段而输入回车，一般不在行尾插入回车）。但visual-line只能在window边缘换行，不够美观。所以加入了visual-fill-column，以实现在fill-column处换行。

打开方法：用 `SPC t L` 打开visual-line-mode即可。可以将下面这行加入到dotspacemacs/config函数中，以实现org-mode自动加载visual-line-mode：

    (add-hook 'org-mode-hook 'turn-on-visual-line-mode t)

## 词库文件<a id="sec-3-5" name="sec-3-5"></a>

这里提供两个词库文件，分别是bigdict.pyim和sogou.pyim。将这两个文件拷贝到 **~/.emacs.d/pyim/dicts** 路径下，根据需要用pyim的词库管理命令选择词库（推荐使用搜狗词库）。

词库文件百度云链接：
-   **bigdict:** <http://pan.baidu.com/s/1eQCO3o2> 密码：1dr6
-   **sogou:** <http://pan.baidu.com/s/1bn53cgZ> 密码：z3vu
