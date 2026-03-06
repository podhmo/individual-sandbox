// Node.js ネイティブType Strippingで直接実行するスクリプト

interface Config {
  name: string;
  version: number;
}

function printConfig(config: Config): void {
  console.log(`App: ${config.name} v${config.version}`);
}

const config: Config = {
  name: "modern-ts-sandbox",
  version: 1,
};

printConfig(config);
console.log("✅ Node.js native TypeScript execution works!");
