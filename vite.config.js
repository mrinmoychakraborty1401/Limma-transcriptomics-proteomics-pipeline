import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  // This is the crucial fix:
  // It changes all asset paths from absolute (/) to relative (./)
  // so they work when loaded with `file://` in Electron.
  base: './',
})