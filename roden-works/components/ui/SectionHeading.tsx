'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

interface SectionHeadingProps {
  title: string
  subtitle?: string
  label?: string
  align?: 'left' | 'center'
  light?: boolean
}

export default function SectionHeading({
  title,
  subtitle,
  label,
  align = 'left',
  light = false,
}: SectionHeadingProps) {
  const { ref, isInView } = useInView(0.3)

  return (
    <div
      ref={ref}
      className={`mb-12 md:mb-16 ${align === 'center' ? 'text-center' : ''}`}
    >
      {label && (
        <motion.span
          initial={{ opacity: 0, y: 10 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.5 }}
          className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-4"
        >
          {label}
        </motion.span>
      )}
      <motion.h2
        initial={{ opacity: 0, y: 20 }}
        animate={isInView ? { opacity: 1, y: 0 } : {}}
        transition={{ duration: 0.6, delay: 0.1 }}
        className={`font-serif text-display ${light ? 'text-slate-950' : 'text-white'}`}
      >
        {title}
      </motion.h2>
      {subtitle && (
        <motion.p
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6, delay: 0.2 }}
          className={`mt-4 text-lg max-w-2xl leading-relaxed ${
            light ? 'text-slate-950/70' : 'text-titanium'
          } ${align === 'center' ? 'mx-auto' : ''}`}
        >
          {subtitle}
        </motion.p>
      )}
    </div>
  )
}
