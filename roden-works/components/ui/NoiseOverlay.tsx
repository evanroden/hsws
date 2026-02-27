'use client'

import { useEffect, useRef } from 'react'

/**
 * Subtle film grain noise overlay using canvas.
 * Uses a 512x512 canvas with smooth scaling for fine, organic grain.
 */
export default function NoiseOverlay({ opacity = 0.025 }: { opacity?: number }) {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const w = 512
    const h = 512
    canvas.width = w
    canvas.height = h

    let frame: number
    let lastTime = 0
    const interval = 1000 / 24 // 24fps — cinematic framerate

    const drawNoise = (time: number) => {
      frame = requestAnimationFrame(drawNoise)

      // Pause rendering when tab is hidden to save CPU/battery
      if (document.hidden) return
      if (time - lastTime < interval) return
      lastTime = time

      const imageData = ctx.createImageData(w, h)
      const data = imageData.data

      for (let i = 0; i < data.length; i += 4) {
        // Gaussian-ish distribution for more natural film grain
        const r1 = Math.random()
        const r2 = Math.random()
        const v = Math.sqrt(-2 * Math.log(r1 || 0.001)) * Math.cos(2 * Math.PI * r2)
        const pixel = 128 + v * 40 // centered at mid-gray with moderate spread

        data[i] = pixel
        data[i + 1] = pixel
        data[i + 2] = pixel
        data[i + 3] = 255
      }

      ctx.putImageData(imageData, 0, 0)
    }

    frame = requestAnimationFrame(drawNoise)
    return () => cancelAnimationFrame(frame)
  }, [])

  return (
    <canvas
      ref={canvasRef}
      className="pointer-events-none fixed inset-0 z-[9990] w-full h-full"
      style={{
        opacity,
        mixBlendMode: 'overlay',
      }}
      aria-hidden="true"
    />
  )
}
