'use client'

import { useRef, useEffect, useState } from 'react'
import { motion, useInView, useAnimation } from 'framer-motion'

/* ─── TextReveal: Word-by-word reveal on scroll ──── */

interface TextRevealProps {
  children: string
  className?: string
  /** Delay before animation starts (seconds) */
  delay?: number
  /** Stagger between words (seconds) */
  stagger?: number
  as?: 'h1' | 'h2' | 'h3' | 'p' | 'span'
}

export function TextReveal({
  children,
  className = '',
  delay = 0,
  stagger = 0.04,
  as: Tag = 'p',
}: TextRevealProps) {
  const ref = useRef<HTMLDivElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-10%' })
  const words = children.split(' ')

  return (
    <Tag ref={ref as React.RefObject<HTMLHeadingElement & HTMLParagraphElement>} className={className}>
      {words.map((word, i) => (
        <span key={i} className="inline-block overflow-hidden mr-[0.3em]">
          <motion.span
            className="inline-block"
            initial={{ y: '110%', rotateX: -80 }}
            animate={isInView ? { y: '0%', rotateX: 0 } : {}}
            transition={{
              duration: 0.6,
              delay: delay + i * stagger,
              ease: [0.16, 1, 0.3, 1],
            }}
          >
            {word}
          </motion.span>
        </span>
      ))}
    </Tag>
  )
}

/* ─── CharacterReveal: Letter-by-letter ──────────── */

interface CharacterRevealProps {
  children: string
  className?: string
  delay?: number
  stagger?: number
  as?: 'h1' | 'h2' | 'h3' | 'p' | 'span'
}

export function CharacterReveal({
  children,
  className = '',
  delay = 0,
  stagger = 0.02,
  as: Tag = 'span',
}: CharacterRevealProps) {
  const ref = useRef<HTMLDivElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-10%' })

  return (
    <Tag ref={ref as React.RefObject<HTMLHeadingElement & HTMLParagraphElement>} className={className} aria-label={children}>
      {children.split('').map((char, i) => (
        <motion.span
          key={i}
          className="inline-block"
          initial={{ opacity: 0, y: 20, filter: 'blur(4px)' }}
          animate={isInView ? { opacity: 1, y: 0, filter: 'blur(0px)' } : {}}
          transition={{
            duration: 0.4,
            delay: delay + i * stagger,
            ease: [0.16, 1, 0.3, 1],
          }}
          aria-hidden="true"
        >
          {char === ' ' ? '\u00A0' : char}
        </motion.span>
      ))}
    </Tag>
  )
}

/* ─── LineReveal: Line-by-line slide up ──────────── */

interface LineRevealProps {
  children: string
  className?: string
  delay?: number
}

export function LineReveal({
  children,
  className = '',
  delay = 0,
}: LineRevealProps) {
  const ref = useRef<HTMLDivElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-5%' })

  return (
    <div ref={ref} className={`overflow-hidden ${className}`}>
      <motion.div
        initial={{ y: '100%' }}
        animate={isInView ? { y: '0%' } : {}}
        transition={{
          duration: 0.8,
          delay,
          ease: [0.16, 1, 0.3, 1],
        }}
      >
        {children}
      </motion.div>
    </div>
  )
}

/* ─── MaskReveal: Clip-path reveal effect ────────── */

interface MaskRevealProps {
  children: React.ReactNode
  className?: string
  delay?: number
  direction?: 'up' | 'left' | 'right'
}

export function MaskReveal({
  children,
  className = '',
  delay = 0,
  direction = 'up',
}: MaskRevealProps) {
  const ref = useRef<HTMLDivElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-5%' })

  const clipPaths = {
    up: {
      initial: 'inset(100% 0% 0% 0%)',
      animate: 'inset(0% 0% 0% 0%)',
    },
    left: {
      initial: 'inset(0% 100% 0% 0%)',
      animate: 'inset(0% 0% 0% 0%)',
    },
    right: {
      initial: 'inset(0% 0% 0% 100%)',
      animate: 'inset(0% 0% 0% 0%)',
    },
  }

  return (
    <motion.div
      ref={ref}
      className={className}
      initial={{ clipPath: clipPaths[direction].initial }}
      animate={isInView ? { clipPath: clipPaths[direction].animate } : {}}
      transition={{
        duration: 1,
        delay,
        ease: [0.16, 1, 0.3, 1],
      }}
    >
      {children}
    </motion.div>
  )
}

/* ─── FadeBlur: Fade in with blur clearing ───────── */

interface FadeBlurProps {
  children: React.ReactNode
  className?: string
  delay?: number
}

export function FadeBlur({
  children,
  className = '',
  delay = 0,
}: FadeBlurProps) {
  const ref = useRef<HTMLDivElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-5%' })

  return (
    <motion.div
      ref={ref}
      className={className}
      initial={{ opacity: 0, y: 30, filter: 'blur(10px)' }}
      animate={isInView ? { opacity: 1, y: 0, filter: 'blur(0px)' } : {}}
      transition={{
        duration: 0.8,
        delay,
        ease: [0.16, 1, 0.3, 1],
      }}
    >
      {children}
    </motion.div>
  )
}

/* ─── Parallax: Scroll-linked parallax ───────────── */

interface ParallaxProps {
  children: React.ReactNode
  className?: string
  speed?: number
}

export function Parallax({
  children,
  className = '',
  speed = 0.3,
}: ParallaxProps) {
  const ref = useRef<HTMLDivElement>(null)
  const [offset, setOffset] = useState(0)

  useEffect(() => {
    const el = ref.current
    if (!el) return

    const handleScroll = () => {
      const rect = el.getBoundingClientRect()
      const windowH = window.innerHeight
      const progress = (windowH - rect.top) / (windowH + rect.height)
      setOffset((progress - 0.5) * speed * 100)
    }

    window.addEventListener('scroll', handleScroll, { passive: true })
    handleScroll()
    return () => window.removeEventListener('scroll', handleScroll)
  }, [speed])

  return (
    <div ref={ref} className={`overflow-hidden ${className}`}>
      <motion.div style={{ y: offset }}>
        {children}
      </motion.div>
    </div>
  )
}
