<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. 特点</a></li>
<li><a href="#orgheadline2">2. 安装</a></li>
<li><a href="#orgheadline6">3. 设置</a>
<ul>
<li><a href="#orgheadline3">3.1. 设置英文字体</a></li>
<li><a href="#orgheadline4">3.2. 设置中文字体</a></li>
<li><a href="#orgheadline5">3.3. 关于中英文字体等宽</a></li>
</ul>
</li>
<li><a href="#orgheadline7">4. Keybindings</a></li>
</ul>
</div>
</div>


# 特点<a id="orgheadline1"></a>

这个private layer作为spacemacs官方[Chinese layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/chinese)的一个替代， **大部份设置都与Chinese layer相同，在它的基础上做了少量修改。** 之所以另外开一个layer，主要是因为官方的layer还不能完全满足我个人的需要。本设置层整合了ELPA中和中文写作有关的包，目的是为使用spacemacs进行中文写作的同学提供帮助，节省大家的时间。其中包含了以下和中文写作有关的包：

-   **ace-pinyin:** 根据汉字的拼音首字母进行ace-jump。
-   **chinese-pyim:** 目前最好用的emacs拼音输入法。
-   **find-by-pinyin-dired:** 根据拼音首字母找到并打开文件。
-   **pangu-spacing:** 以“软空格”的形式在中英混排文档中自动增加汉字与英文单词的间距。
-   **pinyin-search:** 顾名思义，根据汉字拼音首字母进行查找。(not activated by default)
-   **youdao-dictionary:** 有道词典

# 安装<a id="orgheadline2"></a>

安装非常简单，首先

    git clone https://github.com/et2010/Chinese.git ~/.emacs.d/private/han

注意：在没有官方Chinese layer之前，这个layer就叫Chinese，之后为了避免冲突，我将它改名为han。

然后在你的.spacemacs文件中添加han设置层。配置类型推荐选择hybrid：

# 设置<a id="orgheadline6"></a>

## 设置英文字体<a id="orgheadline3"></a>

首先，利用spacemacs自身提供的函数设置英文字体：

    (defun dotspacemacs/init ()
      dotspacemacs-default-font '("Source Code Pro"
                                  :size 14
                                  :weight normal
                                  :width normal
                                  :powerline-scale 1.2)
      )

Spacemacs默认的字体为“Source Code Pro”，默认字号为12. 为了实现中英文混排时字体的等宽，我将字号修改为14.（当然，这是试出来的。）

## 设置中文字体<a id="orgheadline4"></a>

在.spacemacs中添加下面这段代码，用于设置中文字体。这里我用了"Adobe Song Std"，字号（高度）为16px，和可以替换为系统中已安装的任意字体。

    (defun dotspacemacs/user-config ()
      (when window-system
        ;; "CJK Unified Ideographs" (han) U+4E00 - U+9FFF
        (set-fontset-font "fontset-default"
                          (cons (decode-char 'ucs #x4e00)
                                (decode-char 'ucs #x9fff))
                          "-*-Adobe Song Std-*-*-*-*-16-*-*-*-*-*-*-*"))
      )

## 关于中英文字体等宽<a id="orgheadline5"></a>

当使用GUI的emacs时，默认的中文字符宽度一般不等于英文字符宽度的两倍，这样会导致用字符画的表格非常丑。解决办法是分别采用不同字号（高度）的中英文字体，使中文字符的宽度刚好等于英文字符宽度的两倍。通过试验发现不少常用的英文等宽字体字号设为14，中文字号设为16时可以满足宽度要求。（注意这里使用的是绝对字号，不是放大系数。）且中英文字符高度都以像素为单位，因而不论你的屏幕ppi是多少，这样设置都可以满足中英文字体等宽。使用上述字号，以下列出的中英字体任意组合都可以满足等宽：

-   中文字体
    -   **Adobe Fangsong Std**
    -   Adobe Heiti Std
    -   Adobe Kaiti Std
    -   **Adobe Song Std**
    -   **NSimSun**
    -   WenQuanYi Micro Hei Mono
    -   WenQuanYi Zen Hei Mono
    -   **微软雅黑**

-   英文字体（14号)
    -   **Anonymous Pro**
    -   **Bitstream Vera Sans Mono**
    -   BPmono
    -   **Consolas**
    -   Courier 10 Pitch
    -   *Courier New*
    -   Cousine
    -   **DejaVu Sans Mono**
    -   Droid Sans Mono
    -   **Envy Code R**
    -   *FreeMono*
    -   Liberation Mono
    -   **Monaco**
    -   MonospaceTypewriter
    -   *Nimbus Mono L*
    -   NotCourierSans
    -   **Oxygen Mono**
    -   **Source Code Pro**
    -   *TlwgMono*

-   英文字体（16号）
    -   Audimat Mono
    -   **Inconsolata**
    -   monofur
    -   **Ubuntu Mono**

-   英文字体（12号）
    -   *Linux Libertine Mono O*

使用说明：

1.  **加粗字体** 为推荐使用字体， *斜体* 为英文衬线字体。
2.  中文的16号字体可以与英文的16,14,12号字体实现等宽；大部分可以实现与中文16号字体等宽的英文等宽字体都是14号。（不推荐使用英文12号字体，即Linux Libertine Mono O，因为与中文字体高度相差太大）。
3.  不同系统中显示的字体族名不同，比如Adobe宋体，在Linux中显示为"Adobe Song Std", 在Windows中显示为"Adobe 宋体 Std L". 以上字体是在不同操作系统中试验得到的， **具体使用时以你的操作系统中所显示的字体族名为准。**

# Keybindings<a id="orgheadline7"></a>

可以在你的.spacemacs文件中添加以下设置：

    (defun dotspacemacs/user-config ()
      (define-key evil-hybrid-state-map (kbd "C-SPC") 'toggle-input-method)
      (define-key evil-hybrid-state-map (kbd "M-f") 'pyim-forward-word)
      (define-key evil-hybrid-state-map (kbd "M-b") 'pyim-backward-word)
      (define-key evil-hybrid-state-map (kbd "C-s") 'isearch-forward-pinyin)
      (define-key evil-hybrid-state-map (kbd "C-r") 'isearch-backward-pinyin)
      )

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">key</th>
<th scope="col" class="org-left">function</th>
<th scope="col" class="org-left">state</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">`SPC d`</td>
<td class="org-left">ace-pinyin-dwim</td>
<td class="org-left">normal</td>
</tr>


<tr>
<td class="org-left">`SPC o d`</td>
<td class="org-left">find-by-pinyin-dired</td>
<td class="org-left">normal</td>
</tr>


<tr>
<td class="org-left">`SPC o y`</td>
<td class="org-left">youdao-dictionary-search-at-point+</td>
<td class="org-left">normal</td>
</tr>


<tr>
<td class="org-left">`C-SPC`</td>
<td class="org-left">toggle-input-method</td>
<td class="org-left">hybrid</td>
</tr>


<tr>
<td class="org-left">`M-f`</td>
<td class="org-left">pyim-forward-word</td>
<td class="org-left">hybrid</td>
</tr>


<tr>
<td class="org-left">`M-b`</td>
<td class="org-left">pyim-backward-word</td>
<td class="org-left">hybrid</td>
</tr>


<tr>
<td class="org-left">`C-s`</td>
<td class="org-left">isearch-forward-pinyin</td>
<td class="org-left">hybrid</td>
</tr>


<tr>
<td class="org-left">`C-r`</td>
<td class="org-left">isearch-backward-pinyin</td>
<td class="org-left">hybrid</td>
</tr>
</tbody>
</table>
