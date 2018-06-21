## matplotlib backendsの構成を見る

参考はmatplotlib/backends/backend_svg.py

存在しているclass

- XMLWriter
- RendererSVG
- FigureCanvasSVG
- FigureManagerSVG

存在している関数

- escape_cdata
- escape_comment
- escape_attrib
- short_float_fmt
- generate_transform
- generate_css
- new_figure_manager
- new_figure_manager_given_figure

この内 `pylab_setup()` で使われるのは

- new_figure_manager
- show or do_nothing_show
- draw_if_interactive or do_nothing

関数はほとんどヘルパー関数かも

### classについてもう少し詳しく

XMLWriterはただの便利オブジェクト

```
class RendererSVG(matplotlib.backend_bases.RendererBase):
class FigureCanvasSVG(matplotlib.backend_bases.FigureCanvasBase):
class FigureManagerSVG(matplotlib.backend_bases.FigureManagerBase):
```
