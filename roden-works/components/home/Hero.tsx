'use client'

import { motion } from 'framer-motion'
import { useEffect, useState } from 'react'

const words = [
  'Engineering.',
  'Sustainability.',
  'Advocacy.',
  'Cinematography.',
]

export default function Hero() {
  const [currentWord, setCurrentWord] = useState(0)

  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentWord((prev) => (prev + 1) % words.length)
    }, 3000)
    return () => clearInterval(interval)
  }, [])

  return (
    <section className="relative min-h-screen flex items-center justify-center overflow-hidden bg-slate-950">
      {/* Animated background grid */}
      <div className="absolute inset-0">
        <div
          className="absolute inset-0 opacity-[0.04]"
          style={{
            backgroundImage: `
              linear-gradient(rgba(138, 155, 168, 0.3) 1px, transparent 1px),
              linear-gradient(90deg, rgba(138, 155, 168, 0.3) 1px, transparent 1px)
            `,
            backgroundSize: '60px 60px',
          }}
        />
        {/* Radial gradient overlay */}
        <div className="absolute inset-0 bg-gradient-radial from-forest/10 via-transparent to-transparent opacity-50"
          style={{ background: 'radial-gradient(ellipse at 50% 50%, rgba(27,58,45,0.15) 0%, transparent 70%)' }}
        />
      </div>

      {/* Floating particles */}
      <HeroParticles />

      <div className="content-width relative z-10 text-center">
        <motion.div
          initial={{ opacity: 0, scale: 0.95 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ duration: 1, ease: [0.16, 1, 0.3, 1] }}
        >
          <motion.h1
            initial={{ opacity: 0, y: 40 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
            className="font-serif text-display-xl text-white"
          >
            Evan Joseph Roden.
          </motion.h1>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8, delay: 0.6 }}
            className="mt-6 md:mt-8 max-w-3xl mx-auto"
          >
            <p className="text-lg md:text-xl text-titanium leading-relaxed">
              Optimizing complex systems to improve human quality of life
              <br className="hidden md:block" />
              {' '}at the intersection of{' '}
              <span className="relative inline-block min-w-[180px]">
                <motion.span
                  key={currentWord}
                  initial={{ opacity: 0, y: 10 }}
                  animate={{ opacity: 1, y: 0 }}
                  exit={{ opacity: 0, y: -10 }}
                  transition={{ duration: 0.4 }}
                  className="text-copper font-serif italic"
                >
                  {words[currentWord]}
                </motion.span>
              </span>
            </p>
          </motion.div>

          {/* CTA Buttons */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8, delay: 0.9 }}
            className="mt-10 flex flex-col sm:flex-row gap-4 justify-center"
          >
            <a
              href="/engineering-and-sustainability"
              className="inline-flex items-center justify-center px-8 py-3.5 bg-forest-light hover:bg-forest text-white text-sm font-medium rounded-lg transition-all duration-300 hover:scale-105"
            >
              View My Work
            </a>
            <a
              href="/about"
              className="inline-flex items-center justify-center px-8 py-3.5 glass glass-hover text-white text-sm font-medium rounded-lg"
            >
              About Me
            </a>
          </motion.div>
        </motion.div>
      </div>

      {/* Scroll indicator */}
      <motion.div
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 1.5, duration: 0.8 }}
        className="absolute bottom-8 left-1/2 -translate-x-1/2"
      >
        <motion.div
          animate={{ y: [0, 8, 0] }}
          transition={{ repeat: Infinity, duration: 2, ease: 'easeInOut' }}
          className="flex flex-col items-center gap-2"
        >
          <span className="text-titanium/50 text-xs font-mono tracking-widest uppercase">
            Scroll
          </span>
          <div className="w-px h-8 bg-gradient-to-b from-titanium/50 to-transparent" />
        </motion.div>
      </motion.div>
    </section>
  )
}

function HeroParticles() {
  const [particles, setParticles] = useState<Array<{ id: number; x: number; y: number; size: number; delay: number }>>([])

  useEffect(() => {
    const p = Array.from({ length: 30 }, (_, i) => ({
      id: i,
      x: Math.random() * 100,
      y: Math.random() * 100,
      size: Math.random() * 3 + 1,
      delay: Math.random() * 5,
    }))
    setParticles(p)
  }, [])

  return (
    <div className="absolute inset-0 overflow-hidden pointer-events-none">
      {particles.map((p) => (
        <motion.div
          key={p.id}
          className="absolute rounded-full bg-titanium/20"
          style={{
            left: `${p.x}%`,
            top: `${p.y}%`,
            width: p.size,
            height: p.size,
          }}
          animate={{
            y: [0, -30, 0],
            opacity: [0.2, 0.5, 0.2],
          }}
          transition={{
            repeat: Infinity,
            duration: 4 + Math.random() * 3,
            delay: p.delay,
            ease: 'easeInOut',
          }}
        />
      ))}
    </div>
  )
}
