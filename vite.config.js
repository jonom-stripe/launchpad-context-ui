import { defineConfig } from 'vite'
import { plugin as elm } from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elm()],
  base: '/',
  build: {
    // Ensure proper static file serving on Vercel
    outDir: 'dist',
    emptyOutDir: true,
    assetsDir: 'assets'
  }
}) 