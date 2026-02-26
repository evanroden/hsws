'use client'

import { useRef, useState, useCallback } from 'react'
import { motion, useMotionValue, useSpring, useTransform } from 'framer-motion'

interface TiltCardProps {
  children: React.ReactNode
  className?: string
  /** Max tilt angle in degrees */
  maxTilt?: number
  /** Spotlight/glare intensity 0-1 */
  glare?: number
}

/**
 * Interactive 3D tilt card with perspective transform and spotlight glare.
 * Responds to mouse position for a premium, tactile feel.
 */
export default function TiltCard({
  children,
  className = '',
  maxTilt = 8,
  glare = 0.15,
}: TiltCardProps) {
  const ref = useRef<HTMLDivElement>(null)
  const [hovering, setHovering] = useState(false)

  const mouseX = useMotionValue(0.5)
  const mouseY = useMotionValue(0.5)

  const springConfig = { stiffness: 150, damping: 15 }
  const rotateX = useSpring(useTransform(mouseY, [0, 1], [maxTilt, -maxTilt]), springConfig)
  const rotateY = useSpring(useTransform(mouseX, [0, 1], [-maxTilt, maxTilt]), springConfig)

  const glareX = useTransform(mouseX, [0, 1], ['-20%', '120%'])
  const glareY = useTransform(mouseY, [0, 1], ['-20%', '120%'])

  const handleMouseMove = useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      const rect = ref.current?.getBoundingClientRect()
      if (!rect) return
      mouseX.set((e.clientX - rect.left) / rect.width)
      mouseY.set((e.clientY - rect.top) / rect.height)
    },
    [mouseX, mouseY]
  )

  const handleMouseEnter = useCallback(() => setHovering(true), [])
  const handleMouseLeave = useCallback(() => {
    setHovering(false)
    mouseX.set(0.5)
    mouseY.set(0.5)
  }, [mouseX, mouseY])

  return (
    <motion.div
      ref={ref}
      className={`relative ${className}`}
      style={{
        perspective: 1000,
        transformStyle: 'preserve-3d',
      }}
      onMouseMove={handleMouseMove}
      onMouseEnter={handleMouseEnter}
      onMouseLeave={handleMouseLeave}
    >
      <motion.div
        style={{
          rotateX,
          rotateY,
          transformStyle: 'preserve-3d',
        }}
        animate={{
          scale: hovering ? 1.02 : 1,
        }}
        transition={{ scale: { duration: 0.2 } }}
        className="relative w-full h-full"
      >
        {children}

        {/* Spotlight glare */}
        {glare > 0 && (
          <motion.div
            className="absolute inset-0 rounded-2xl pointer-events-none z-10"
            style={{
              background: `radial-gradient(circle at ${glareX}px ${glareY}px, rgba(255,255,255,${glare}), transparent 60%)`,
              opacity: hovering ? 1 : 0,
            }}
            animate={{ opacity: hovering ? 1 : 0 }}
            transition={{ duration: 0.3 }}
          />
        )}
      </motion.div>
    </motion.div>
  )
}
