from distutils.core import setup

setup(
    name="myapp2",
    version="0.0.0",
    data_files=["main.py"],  # 真面目にモジュールを分けた場合にはpackages=find_packages()を使うかもしれない
    install_requires=["fastapi", "uvicorn"],
    entry_points="""
[console_scripts]
myapp2 = main:main
""",
)
