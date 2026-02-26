'use client'

import { motion } from 'framer-motion'

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
  const bgClass = {
    dark: 'bg-slate-950',
    forest: 'bg-gradient-to-br from-slate-950 via-forest/30 to-slate-950',
    warm: 'bg-gradient-to-br from-slate-950 via-copper/10 to-slate-950',
  }[variant]

  return (
    <section className={`relative min-h-[60vh] flex items-end ${bgClass}`}>
      {/* Background pattern */}
      <div className="absolute inset-0 overflow-hidden">
        <div className="absolute inset-0 opacity-[0.03]"
          style={{
            backgroundImage: `radial-gradient(circle at 1px 1px, white 1px, transparent 1px)`,
            backgroundSize: '40px 40px',
          }}
        />
      </div>

      <div className="content-width relative z-10 pb-16 md:pb-24 pt-32">
        {label && (
          <motion.span
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-4"
          >
            {label}
          </motion.span>
        )}
        <motion.h1
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.7, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
          className="font-serif text-display-xl text-white max-w-4xl"
        >
          {title}
        </motion.h1>
        <motion.p
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.7, delay: 0.5 }}
          className="mt-6 text-lg md:text-xl text-titanium max-w-2xl leading-relaxed"
        >
          {subtitle}
        </motion.p>
      </div>
    </section>
  )
}
