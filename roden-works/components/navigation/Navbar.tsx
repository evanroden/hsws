'use client'

import { useState, useEffect } from 'react'
import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { motion, AnimatePresence } from 'framer-motion'
import { NAV_ITEMS } from '@/lib/constants'

export default function Navbar() {
  const [scrolled, setScrolled] = useState(false)
  const [mobileOpen, setMobileOpen] = useState(false)
  const [hidden, setHidden] = useState(false)
  const pathname = usePathname()

  useEffect(() => {
    let lastY = 0
    const handleScroll = () => {
      const y = window.scrollY
      setScrolled(y > 50)
      if (y > 300) {
        setHidden(y > lastY && y - lastY > 5)
      } else {
        setHidden(false)
      }
      lastY = y
    }
    window.addEventListener('scroll', handleScroll, { passive: true })
    return () => window.removeEventListener('scroll', handleScroll)
  }, [])

  useEffect(() => {
    setMobileOpen(false)
  }, [pathname])

  return (
    <>
      <motion.header
        initial={{ y: -100 }}
        animate={{ y: hidden && !mobileOpen ? -100 : 0 }}
        transition={{ duration: 0.3, ease: [0.16, 1, 0.3, 1] }}
        className={`fixed top-0 left-0 right-0 z-50 transition-all duration-500 ${
          scrolled
            ? 'bg-slate-950/70 backdrop-blur-2xl border-b border-white/[0.06] shadow-lg shadow-black/20'
            : 'bg-transparent'
        }`}
      >
        <nav className="content-width flex items-center justify-between h-16 md:h-20">
          <Link
            href="/"
            className="relative font-serif text-xl md:text-2xl tracking-tight font-bold text-white group"
            data-cursor="Home"
          >
            <span className="relative z-10 group-hover:text-copper transition-colors duration-300">
              RODEN
            </span>
          </Link>

          <div className="hidden md:flex items-center gap-1">
            {NAV_ITEMS.map((item) => {
              const isActive =
                item.href === '/'
                  ? pathname === '/'
                  : pathname.startsWith(item.href)
              return (
                <Link
                  key={item.href}
                  href={item.href}
                  data-cursor="View"
                  className={`relative px-4 py-2 text-sm font-sans tracking-wide transition-colors rounded-lg ${
                    isActive ? 'text-white' : 'text-titanium hover:text-white'
                  }`}
                >
                  {isActive && (
                    <motion.div
                      layoutId="nav-pill"
                      className="absolute inset-0 bg-white/[0.06] rounded-lg"
                      transition={{ type: 'spring', stiffness: 400, damping: 30 }}
                    />
                  )}
                  <span className="relative z-10">{item.label}</span>
                </Link>
              )
            })}
          </div>

          <button
            onClick={() => setMobileOpen(!mobileOpen)}
            className="md:hidden relative w-10 h-10 flex flex-col justify-center items-center gap-1.5 rounded-lg hover:bg-white/5 transition-colors"
            aria-label={mobileOpen ? 'Close menu' : 'Open menu'}
          >
            <motion.span
              animate={mobileOpen ? { rotate: 45, y: 5 } : { rotate: 0, y: 0 }}
              className="block w-5 h-px bg-white origin-center"
            />
            <motion.span
              animate={mobileOpen ? { opacity: 0, scaleX: 0 } : { opacity: 1, scaleX: 1 }}
              className="block w-5 h-px bg-white"
            />
            <motion.span
              animate={mobileOpen ? { rotate: -45, y: -5 } : { rotate: 0, y: 0 }}
              className="block w-5 h-px bg-white origin-center"
            />
          </button>
        </nav>
      </motion.header>

      <AnimatePresence>
        {mobileOpen && (
          <motion.div
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
            transition={{ duration: 0.3 }}
            className="fixed inset-0 z-40 bg-slate-950/98 backdrop-blur-2xl md:hidden"
          >
            <nav className="flex flex-col items-center justify-center h-full gap-2">
              {NAV_ITEMS.map((item, i) => {
                const isActive =
                  item.href === '/' ? pathname === '/' : pathname.startsWith(item.href)
                return (
                  <motion.div
                    key={item.href}
                    initial={{ opacity: 0, y: 30, filter: 'blur(4px)' }}
                    animate={{ opacity: 1, y: 0, filter: 'blur(0px)' }}
                    exit={{ opacity: 0, y: 30 }}
                    transition={{ delay: i * 0.08, duration: 0.5, ease: [0.16, 1, 0.3, 1] }}
                  >
                    <Link
                      href={item.href}
                      className={`block py-3 px-6 font-serif text-4xl transition-colors duration-300 ${
                        isActive ? 'text-copper' : 'text-white hover:text-copper'
                      }`}
                      onClick={() => setMobileOpen(false)}
                    >
                      {item.label}
                    </Link>
                  </motion.div>
                )
              })}
              <motion.div
                initial={{ scaleX: 0 }}
                animate={{ scaleX: 1 }}
                transition={{ delay: 0.5, duration: 0.8 }}
                className="mt-8 h-px w-24 bg-gradient-to-r from-transparent via-copper/40 to-transparent"
              />
            </nav>
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
}
