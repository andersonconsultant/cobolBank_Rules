module.exports = {
    apps : [{
      name: 'index',
      script: 'npm',
      args: 'start index.js',
      watch: true,
      ignore_watch: ['node_modules'],
      env: {
        NODE_ENV: 'production'
      }
    }]
  };