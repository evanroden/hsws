'use client'

import { motion, AnimatePresence, useScroll, useTransform, useMotionValueEvent } from 'framer-motion'
import { useEffect, useState, useRef, useMemo, useCallback } from 'react'
import Link from 'next/link'

const words = [
  'Engineering.',
  'Sustainability.',
  'Advocacy.',
  'Cinematography.',
]

/* ─── Animated gradient mesh background ──────────── */

function GradientMesh() {
  return (
    <div className="absolute inset-0 overflow-hidden">
      {/* Primary orb — forest green */}
      <motion.div
        className="absolute w-[800px] h-[800px] rounded-full"
        style={{
          background: 'radial-gradient(circle, rgba(27,58,45,0.25) 0%, transparent 70%)',
          filter: 'blur(80px)',
        }}
        animate={{
          x: ['-10%', '10%', '-5%'],
          y: ['-5%', '15%', '-10%'],
        }}
        transition={{
          duration: 20,
          repeat: Infinity,
          repeatType: 'reverse',
          ease: 'easeInOut',
        }}
        initial={{ x: '20%', y: '10%' }}
      />

      {/* Secondary orb — copper warmth */}
      <motion.div
        className="absolute w-[600px] h-[600px] rounded-full"
        style={{
          background: 'radial-gradient(circle, rgba(184,115,51,0.1) 0%, transparent 70%)',
          filter: 'blur(100px)',
        }}
        animate={{
          x: ['60%', '40%', '55%'],
          y: ['20%', '50%', '30%'],
        }}
        transition={{
          duration: 25,
          repeat: Infinity,
          repeatType: 'reverse',
          ease: 'easeInOut',
        }}
        initial={{ x: '60%', y: '40%' }}
      />

      {/* Tertiary orb — cold titanium */}
      <motion.div
        className="absolute w-[500px] h-[500px] rounded-full"
        style={{
          background: 'radial-gradient(circle, rgba(138,155,168,0.08) 0%, transparent 70%)',
          filter: 'blur(60px)',
        }}
        animate={{
          x: ['30%', '50%', '35%'],
          y: ['60%', '40%', '70%'],
        }}
        transition={{
          duration: 18,
          repeat: Infinity,
          repeatType: 'reverse',
          ease: 'easeInOut',
        }}
        initial={{ x: '30%', y: '60%' }}
      />
    </div>
  )
}

/* ─── Interactive grid that fades with distance ───── */

function HeroGrid() {
  return (
    <div className="absolute inset-0 overflow-hidden">
      <div
        className="absolute inset-0 opacity-[0.06]"
        style={{
          backgroundImage: `
            linear-gradient(rgba(138, 155, 168, 0.4) 1px, transparent 1px),
            linear-gradient(90deg, rgba(138, 155, 168, 0.4) 1px, transparent 1px)
          `,
          backgroundSize: '80px 80px',
          maskImage: 'radial-gradient(ellipse at 50% 50%, black 20%, transparent 70%)',
          WebkitMaskImage: 'radial-gradient(ellipse at 50% 50%, black 20%, transparent 70%)',
        }}
      />
    </div>
  )
}

/* ─── Particle field with mouse parallax ──────────── */

function ParticleField({ mouseX, mouseY }: { mouseX: number; mouseY: number }) {
  const particles = useMemo(
    () =>
      Array.from({ length: 60 }, (_, i) => ({
        id: i,
        x: Math.random() * 100,
        y: Math.random() * 100,
        size: Math.random() * 2.5 + 0.5,
        duration: 5 + Math.random() * 8,
        delay: Math.random() * 5,
        opacity: Math.random() * 0.4 + 0.1,
        depth: Math.random() * 0.5 + 0.5,
      })),
    []
  )

  return (
    <div className="absolute inset-0 overflow-hidden pointer-events-none">
      {particles.map((p) => (
        <motion.div
          key={p.id}
          className="absolute rounded-full bg-white"
          style={{
            left: `${p.x}%`,
            top: `${p.y}%`,
            width: p.size,
            height: p.size,
            transform: `translate(${(mouseX - 0.5) * 20 * p.depth}px, ${(mouseY - 0.5) * 20 * p.depth}px)`,
          }}
          animate={{
            y: [0, -40, 0],
            x: [0, (Math.random() - 0.5) * 20, 0],
            opacity: [p.opacity * 0.5, p.opacity, p.opacity * 0.5],
          }}
          transition={{
            repeat: Infinity,
            duration: p.duration,
            delay: p.delay,
            ease: 'easeInOut',
          }}
        />
      ))}
    </div>
  )
}

/* ─── Horizontal rule animation ──────────────────── */

function AnimatedRule({ delay = 0 }: { delay?: number }) {
  return (
    <motion.div
      className="h-px w-full bg-gradient-to-r from-transparent via-copper/40 to-transparent"
      initial={{ scaleX: 0, opacity: 0 }}
      animate={{ scaleX: 1, opacity: 1 }}
      transition={{ duration: 1.2, delay, ease: [0.16, 1, 0.3, 1] }}
    />
  )
}

/* ─── Main Hero ──────────────────────────────────── */

export default function Hero() {
  const [currentWord, setCurrentWord] = useState(0)
  const [mousePos, setMousePos] = useState({ x: 0.5, y: 0.5 })
  const [scrollIndicatorVisible, setScrollIndicatorVisible] = useState(true)
  const sectionRef = useRef<HTMLElement>(null)

  const { scrollYProgress } = useScroll({
    target: sectionRef,
    offset: ['start start', 'end start'],
  })

  const heroOpacity = useTransform(scrollYProgress, [0, 0.5], [1, 0])
  const heroScale = useTransform(scrollYProgress, [0, 0.5], [1, 0.95])
  const heroY = useTransform(scrollYProgress, [0, 0.5], [0, 60])

  useMotionValueEvent(scrollYProgress, 'change', (v) => {
    setScrollIndicatorVisible(v < 0.05)
  })

  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentWord((prev) => (prev + 1) % words.length)
    }, 2500)
    return () => clearInterval(interval)
  }, [])

  const handleMouseMove = useCallback((e: React.MouseEvent) => {
    if (!sectionRef.current) return
    const rect = sectionRef.current.getBoundingClientRect()
    setMousePos({
      x: (e.clientX - rect.left) / rect.width,
      y: (e.clientY - rect.top) / rect.height,
    })
  }, [])

  return (
    <section
      ref={sectionRef}
      onMouseMove={handleMouseMove}
      className="relative min-h-screen flex items-center justify-center overflow-hidden bg-slate-950"
    >
      <GradientMesh />
      <HeroGrid />
      <ParticleField mouseX={mousePos.x} mouseY={mousePos.y} />

      {/* Content with scroll-linked parallax */}
      <motion.div
        className="content-width relative z-10 text-center"
        style={{
          opacity: heroOpacity,
          scale: heroScale,
          y: heroY,
        }}
      >
        {/* Overline */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.2 }}
          className="mb-6"
        >
          <span className="inline-flex items-center gap-3 font-mono text-xs tracking-[0.3em] uppercase text-titanium/60">
            <span className="w-8 h-px bg-copper/50" />
            Portfolio
            <span className="w-8 h-px bg-copper/50" />
          </span>
        </motion.div>

        {/* Name — character-level stagger */}
        <h1 className="font-serif text-display-xl text-white overflow-hidden">
          {'Evan Roden.'.split('').map((char, i) => (
            <motion.span
              key={i}
              className="inline-block"
              initial={{ y: '120%', opacity: 0 }}
              animate={{ y: '0%', opacity: 1 }}
              transition={{
                duration: 0.6,
                delay: 0.4 + i * 0.025,
                ease: [0.16, 1, 0.3, 1],
              }}
            >
              {char === ' ' ? '\u00A0' : char}
            </motion.span>
          ))}
        </h1>

        <motion.div
          initial={{ scaleX: 0 }}
          animate={{ scaleX: 1 }}
          transition={{ duration: 1, delay: 0.9, ease: [0.16, 1, 0.3, 1] }}
          className="mx-auto mt-6 mb-8 h-px w-48 bg-gradient-to-r from-transparent via-copper/50 to-transparent"
        />

        {/* Tagline with rotating word */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.8, delay: 1 }}
          className="max-w-3xl mx-auto"
        >
          <p className="text-lg md:text-xl text-titanium leading-relaxed">
            Optimizing complex systems to improve human quality of life
            <br className="hidden md:block" />
            {' '}at the intersection of{' '}
            <span className="relative inline-block min-w-[200px] text-left overflow-hidden align-bottom" style={{ height: '1.4em' }}>
              <AnimatePresence mode="wait">
                <motion.span
                  key={currentWord}
                  initial={{ opacity: 0, y: '100%' }}
                  animate={{ opacity: 1, y: '0%' }}
                  exit={{ opacity: 0, y: '-100%' }}
                  transition={{ duration: 0.5, ease: [0.16, 1, 0.3, 1] }}
                  className="text-copper font-serif italic absolute left-0"
                >
                  {words[currentWord]}
                </motion.span>
              </AnimatePresence>
            </span>
          </p>
        </motion.div>

        {/* CTA Buttons */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.8, delay: 1.3 }}
          className="mt-10 flex flex-col sm:flex-row gap-4 justify-center"
        >
          <Link
            href="/engineering-and-sustainability"
            data-cursor="Explore"
            className="group relative inline-flex items-center justify-center px-8 py-3.5 bg-forest-light text-white text-sm font-medium rounded-lg transition-all duration-300 overflow-hidden"
          >
            <span className="relative z-10">View My Work</span>
            <motion.span
              className="absolute inset-0 bg-forest"
              initial={{ x: '-100%' }}
              whileHover={{ x: '0%' }}
              transition={{ duration: 0.3, ease: [0.16, 1, 0.3, 1] }}
            />
          </Link>
          <Link
            href="/about"
            data-cursor="About"
            className="group relative inline-flex items-center justify-center px-8 py-3.5 glass text-white text-sm font-medium rounded-lg transition-all duration-300 overflow-hidden hover:border-white/20"
          >
            <span className="relative z-10">About Me</span>
          </Link>
        </motion.div>
      </motion.div>

      {/* Scroll indicator — animated chevron */}
      <AnimatePresence>
        {scrollIndicatorVisible && (
          <motion.div
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: 10 }}
            transition={{ duration: 0.4 }}
            className="absolute bottom-10 left-1/2 -translate-x-1/2 z-10 flex flex-col items-center gap-2"
          >
            <motion.div
              animate={{ y: [0, 6, 0] }}
              transition={{ repeat: Infinity, duration: 1.5, ease: 'easeInOut' }}
            >
              <svg
                className="w-6 h-6 text-titanium/40"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
                strokeWidth="1.5"
              >
                <path strokeLinecap="round" strokeLinejoin="round" d="M19 9l-7 7-7-7" />
              </svg>
            </motion.div>
          </motion.div>
        )}
      </AnimatePresence>
    </section>
  )
}
