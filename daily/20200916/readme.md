## make

- https://qiita.com/zawawahoge/items/4b15af9cc556ae10c737
- 初見殺し？ https://qiita.com/zawawahoge/items/4b15af9cc556ae10c737

## 任意の背景に対して読みやすい色

- https://kuroeveryday.blogspot.com/2018/12/get-automatically-readable-font-color-with-sass.html
- https://katashin.info/2018/12/18/247
- https://www.w3.org/WAI/WCAG21/quickref/?versions=2.0#qr-visual-audio-contrast-contrast
- https://www.w3.org/TR/WCAG20/#contrast-ratiodef

## pythonと数式


pythonの関数定義をlatexの記法に変えるのってなんだっけ？
sympy?

- https://jupyter-notebook.readthedocs.io/en/stable/examples/Notebook/Typesetting%20Equations.html
- https://docs.sympy.org/latest/modules/parsing.html
- https://docs.sympy.org/latest/tutorial/printing.html
- https://stackoverflow.com/questions/3867028/converting-a-python-numeric-expression-to-latex
- https://pytexit.readthedocs.io/en/latest/
- https://timmurphy.org/2013/08/28/math-product-symbol-in-latex/

なんか違う気がする？なんだっけ？
`\prod_{n=1}^10 n^2` みたいなのには対応していないかも。。

## githubのものを利用？

https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Cbegin%7Balign%2A%7D%0AR%28g%29+%26%3D+%5Cfrac%7B1%7D%7Bn%7D+%5Csum_%7Bi%3D1%7D%5E%7Bn%7D+%5Cell%28y_i%2Cg%28x_i%29%29%5C%5C%0A%26%3D%5Cfrac%7B1%7D%7B2n%7D+%28%5Cmathbf%7BX%7D%5Cboldsymbol%7Bw%7D-%5Cmathbf%7By%7D%29%5ET+%28%5Cmathbf%7BX%7D%5Cboldsymbol%7Bw%7D-%5Cmathbf%7Bx%7D%29%0A%5Cend%7Balign%2A%7D%0A

a.svg

![\begin{align*}
R(g) &= \frac{1}{n} \sum_{i=1}^{n} \ell(y_i,g(x_i))\\
&=\frac{1}{2n} (\mathbf{X}\boldsymbol{w}-\mathbf{y})^T (\mathbf{X}\boldsymbol{w}-\mathbf{y})
\end{align*}
](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+%5Cbegin%7Balign%2A%7D%0AR%28g%29+%26%3D+%5Cfrac%7B1%7D%7Bn%7D+%5Csum_%7Bi%3D1%7D%5E%7Bn%7D+%5Cell%28y_i%2Cg%28x_i%29%29%5C%5C%0A%26%3D%5Cfrac%7B1%7D%7B2n%7D+%28%5Cmathbf%7BX%7D%5Cboldsymbol%7Bw%7D-%5Cmathbf%7By%7D%29%5ET+%28%5Cmathbf%7BX%7D%5Cboldsymbol%7Bw%7D-%5Cmathbf%7By%7D%29%0A%5Cend%7Balign%2A%7D%0A)

- https://github.com/connorferster/handcalcs

## sympy

- https://note.nkmk.me/python-sympy-factorization-solve-equation/
- http://www.turbare.net/transl/scipy-lecture-notes/packages/sympy.html
- https://home.hirosaki-u.ac.jp/jupyter/sympy/
- https://qiita.com/zawawahoge/items/1be137a8147902a5e6cb
- https://pianofisica.hatenablog.com/entry/2019/04/08/070020#%E8%A1%8C%E5%88%97%E3%81%AE%E8%AB%B8%E6%93%8D%E4%BD%9C

最適化

- http://www.turbare.net/transl/scipy-lecture-notes/advanced/mathematical_optimization/index.html

### 表示

```python
# 第一引数: レンダリングしない
# str_printer: 出力結果をsy.latexでラップして、latex出力に変換する
sympy.init_printing(False, str_printer=sympy.latex)
```

## sympy で表示をカスタマイズ

- https://docs.sympy.org/latest/modules/printing.html

## 最小二乗法

- https://daily-tech.hatenablog.com/entry/2018/03/27/064626
- http://www.fmaj7b5.info/wiki/index.php?title=%E6%9C%80%E5%B0%8F%E4%BA%8C%E4%B9%97%E6%B3%95%E3%81%A8%E3%81%8B



