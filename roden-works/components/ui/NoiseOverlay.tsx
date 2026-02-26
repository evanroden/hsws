'use client'

import { useEffect, useRef } from 'react'

/**
 * Animated film grain noise overlay using canvas.
 * Renders at low resolution for performance, scaled up for texture.
 */
export default function NoiseOverlay({ opacity = 0.035 }: { opacity?: number }) {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    // Small canvas scaled up via CSS for performance
    const w = 128
    const h = 128
    canvas.width = w
    canvas.height = h

    let frame: number
    let lastTime = 0
    const interval = 1000 / 15 // ~15fps for film-like grain

    const drawNoise = (time: number) => {
      frame = requestAnimationFrame(drawNoise)

      if (time - lastTime < interval) return
      lastTime = time

      const imageData = ctx.createImageData(w, h)
      const data = imageData.data

      for (let i = 0; i < data.length; i += 4) {
        const v = Math.random() * 255
        data[i] = v
        data[i + 1] = v
        data[i + 2] = v
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
        imageRendering: 'pixelated',
      }}
      aria-hidden="true"
    />
  )
}
