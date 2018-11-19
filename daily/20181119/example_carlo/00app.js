const carlo = require('carlo')

const bootstrap = async () => {
  const app = await carlo.launch()
  await app.exposeFunction('cwd', () => process.cwd())
  app.serveFolder('./dist')
  await app.load('index.html')
}

bootstrap()
