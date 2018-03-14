// https://qiita.com/carotene512/items/942bef4c7b1529cff572

class ChikaIdle {
  constructor(){
    this.fans = [];
  }

  addFan(fan) {
    this.fans.push(fan);
  }

  noticeCdRelease(msg) {
    console.log(msg);
    /*
     * アイドルはファンの名前は誰も知らない。
     * 知っているのはCDを買ってくれるということだけ
     */
    this.fans.forEach((fan) => fan.getCd());
  }
}

class Otaku {
  constructor(name){
    this.name = name;
  }

  getCd(){
    console.log(this.name + 'はCDをぜってーかうぜ！！');
  }
}


const idle = new ChikaIdle();
const fans = [
  new Otaku('takkun'),
  new Otaku('shinobun'),
  new Otaku('yukki'),
  new Otaku('chi'),
  new Otaku('luna'),
  new Otaku('hoge1'),
  new Otaku('hoge2'),
  new Otaku('hoge3'),
  new Otaku('hoge4'),
  new Otaku('hoge5')
];
fans.forEach(fan => idle.addFan(fan))
idle.noticeCdRelease('みんなだいすき！CD買ってね！');
