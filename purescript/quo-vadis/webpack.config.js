"use strict";

const path = require("path");

const webpack = require("webpack");

const isWebpackDevServer = process.argv.filter(
  a => path.basename(a) === "webpack-dev-server"
).length;

const isWatch = process.argv.filter(a => a === "--watch").length;

const plugins =
  isWebpackDevServer || !isWatch
    ? []
    : [
        function() {
          this.plugin("done", function(stats) {
            process.stderr.write(stats.toString("errors-only"));
          });
        }
      ];

module.exports = {
  devtool: "eval-source-map",

  devServer: {
    contentBase: ".",
    port: 4008,
    stats: "errors-only"
  },

  entry: "./src/Main.js",

  output: {
    path: path.resolve("../../static/js"),
    pathinfo: true,
    filename: "quo-vadis.js"
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              src: [
                "bower_components/purescript-*/src/**/*.purs",
                "src/**/*.purs"
              ],
              bundle: false,
              psc: "psa",
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      },
      {
        test: /\.(png|jpg|gif)$/,
        use: [
          {
            loader: "url-loader"
          }
        ]
      },
      {
        test: /\.scss$/,
        use: [
          {
            loader: "style-loader" // creates style nodes from JS strings
          },
          {
            loader: "css-loader" // translates CSS into CommonJS
          },
          {
            loader: "sass-loader" // compiles Sass to CSS
          }
        ]
      }
    ]
  },

  resolve: {
    modules: ["node_modules", "bower_components"],
    extensions: [".purs", ".js"]
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),
    new webpack.DefinePlugin({
      "process.env": {
        API: JSON.stringify("http://localhost:3000"),
        WEBSOCKET: JSON.stringify("ws://localhost:3000")
      }
    })
  ].concat(plugins)
};
