'use client'

import { useEffect, useRef, useState } from 'react'
import { motion, useSpring } from 'framer-motion'

export default function CustomCursor() {
  const [visible, setVisible] = useState(false)
  const [hovering, setHovering] = useState(false)
  const [clicking, setClicking] = useState(false)
  const [label, setLabel] = useState<string | null>(null)
  const cursorX = useSpring(0, { stiffness: 500, damping: 28 })
  const cursorY = useSpring(0, { stiffness: 500, damping: 28 })
  const dotX = useSpring(0, { stiffness: 2000, damping: 50 })
  const dotY = useSpring(0, { stiffness: 2000, damping: 50 })
  const isTouch = useRef(false)

  useEffect(() => {
    // Detect touch devices
    const touchCheck = () => { isTouch.current = true }
    window.addEventListener('touchstart', touchCheck, { once: true })

    const moveCursor = (e: MouseEvent) => {
      if (isTouch.current) return
      setVisible(true)
      cursorX.set(e.clientX)
      cursorY.set(e.clientY)
      dotX.set(e.clientX)
      dotY.set(e.clientY)
    }

    const handleMouseDown = () => setClicking(true)
    const handleMouseUp = () => setClicking(false)
    const handleMouseLeave = () => setVisible(false)
    const handleMouseEnter = () => { if (!isTouch.current) setVisible(true) }

    // Hover detection for interactive elements
    const handleOverInteractive = (e: Event) => {
      const target = e.target as HTMLElement
      const interactive = target.closest(
        'a, button, [role="button"], input, textarea, select, [data-cursor]'
      )
      if (interactive) {
        setHovering(true)
        const cursorLabel = interactive.getAttribute('data-cursor')
        setLabel(cursorLabel)
      }
    }

    const handleOutInteractive = () => {
      setHovering(false)
      setLabel(null)
    }

    window.addEventListener('mousemove', moveCursor)
    window.addEventListener('mousedown', handleMouseDown)
    window.addEventListener('mouseup', handleMouseUp)
    document.documentElement.addEventListener('mouseleave', handleMouseLeave)
    document.documentElement.addEventListener('mouseenter', handleMouseEnter)
    document.addEventListener('mouseover', handleOverInteractive)
    document.addEventListener('mouseout', handleOutInteractive)

    return () => {
      window.removeEventListener('mousemove', moveCursor)
      window.removeEventListener('mousedown', handleMouseDown)
      window.removeEventListener('mouseup', handleMouseUp)
      document.documentElement.removeEventListener('mouseleave', handleMouseLeave)
      document.documentElement.removeEventListener('mouseenter', handleMouseEnter)
      document.removeEventListener('mouseover', handleOverInteractive)
      document.removeEventListener('mouseout', handleOutInteractive)
      window.removeEventListener('touchstart', touchCheck)
    }
  }, [cursorX, cursorY, dotX, dotY])

  // Don't render on touch devices / SSR
  if (typeof window === 'undefined') return null

  return (
    <>
      {/* Outer ring */}
      <motion.div
        className="fixed top-0 left-0 pointer-events-none z-[9998] hidden md:flex items-center justify-center mix-blend-difference"
        style={{
          x: cursorX,
          y: cursorY,
          translateX: '-50%',
          translateY: '-50%',
        }}
        animate={{
          width: hovering ? (label ? 80 : 48) : 32,
          height: hovering ? (label ? 80 : 48) : 32,
          opacity: visible ? 1 : 0,
          scale: clicking ? 0.85 : 1,
        }}
        transition={{ type: 'spring', stiffness: 300, damping: 20 }}
      >
        <div
          className={`w-full h-full rounded-full border transition-colors duration-200 ${
            hovering ? 'border-white bg-white/10' : 'border-white/40'
          }`}
        />
        {label && hovering && (
          <span className="absolute font-mono text-[9px] tracking-widest uppercase text-white">
            {label}
          </span>
        )}
      </motion.div>

      {/* Inner dot */}
      <motion.div
        className="fixed top-0 left-0 pointer-events-none z-[9999] hidden md:block"
        style={{
          x: dotX,
          y: dotY,
          translateX: '-50%',
          translateY: '-50%',
        }}
        animate={{
          width: hovering ? 4 : 6,
          height: hovering ? 4 : 6,
          opacity: visible ? 1 : 0,
        }}
        transition={{ type: 'spring', stiffness: 500, damping: 28 }}
      >
        <div className="w-full h-full rounded-full bg-white mix-blend-difference" />
      </motion.div>
    </>
  )
}
