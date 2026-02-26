'use client'

import { useRef } from 'react'
import { motion, useScroll, useTransform } from 'framer-motion'

interface PageHeroProps {
  title: string
  subtitle: string
  label?: string
  variant?: 'dark' | 'forest' | 'warm'
}

export default function PageHero({
  title,
  subtitle,
  label,
  variant = 'dark',
}: PageHeroProps) {
  const ref = useRef<HTMLElement>(null)
  const { scrollYProgress } = useScroll({
    target: ref,
    offset: ['start start', 'end start'],
  })

  const opacity = useTransform(scrollYProgress, [0, 0.5], [1, 0])
  const y = useTransform(scrollYProgress, [0, 0.5], [0, 40])

  const bgClass = {
    dark: 'bg-slate-950',
    forest: 'bg-gradient-to-br from-slate-950 via-forest/30 to-slate-950',
    warm: 'bg-gradient-to-br from-slate-950 via-copper/10 to-slate-950',
  }[variant]

  return (
    <section ref={ref} className={`relative min-h-[60vh] flex items-end ${bgClass} overflow-hidden`}>
      {/* Background pattern with mask */}
      <div className="absolute inset-0 overflow-hidden">
        <div
          className="absolute inset-0 opacity-[0.04]"
          style={{
            backgroundImage: `radial-gradient(circle at 1px 1px, white 1px, transparent 1px)`,
            backgroundSize: '40px 40px',
            maskImage: 'radial-gradient(ellipse at 50% 80%, black 30%, transparent 70%)',
            WebkitMaskImage: 'radial-gradient(ellipse at 50% 80%, black 30%, transparent 70%)',
          }}
        />

        {/* Gradient orb */}
        <motion.div
          className="absolute w-[600px] h-[600px] rounded-full"
          style={{
            background:
              variant === 'forest'
                ? 'radial-gradient(circle, rgba(27,58,45,0.2) 0%, transparent 70%)'
                : variant === 'warm'
                ? 'radial-gradient(circle, rgba(184,115,51,0.1) 0%, transparent 70%)'
                : 'radial-gradient(circle, rgba(138,155,168,0.06) 0%, transparent 70%)',
            filter: 'blur(60px)',
            right: '-10%',
            top: '10%',
          }}
          animate={{
            x: [0, 30, 0],
            y: [0, -20, 0],
          }}
          transition={{
            duration: 15,
            repeat: Infinity,
            repeatType: 'reverse',
            ease: 'easeInOut',
          }}
        />
      </div>

      <motion.div
        className="content-width relative z-10 pb-16 md:pb-24 pt-32"
        style={{ opacity, y }}
      >
        {label && (
          <motion.div
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="flex items-center gap-3 mb-5"
          >
            <span className="w-8 h-px bg-copper/60" />
            <span className="font-mono text-xs tracking-[0.2em] uppercase text-copper">
              {label}
            </span>
          </motion.div>
        )}

        <div className="overflow-hidden">
          <motion.h1
            initial={{ y: '100%' }}
            animate={{ y: '0%' }}
            transition={{ duration: 0.7, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
            className="font-serif text-display-xl text-white max-w-4xl"
          >
            {title}
          </motion.h1>
        </div>

        <motion.p
          initial={{ opacity: 0, y: 20, filter: 'blur(4px)' }}
          animate={{ opacity: 1, y: 0, filter: 'blur(0px)' }}
          transition={{ duration: 0.7, delay: 0.6 }}
          className="mt-6 text-lg md:text-xl text-titanium max-w-2xl leading-relaxed"
        >
          {subtitle}
        </motion.p>

        {/* Animated underline */}
        <motion.div
          initial={{ scaleX: 0 }}
          animate={{ scaleX: 1 }}
          transition={{ duration: 1, delay: 0.9, ease: [0.16, 1, 0.3, 1] }}
          className="mt-8 h-px w-24 bg-gradient-to-r from-copper/60 to-transparent origin-left"
        />
      </motion.div>
    </section>
  )
}
