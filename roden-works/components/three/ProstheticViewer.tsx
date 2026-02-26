'use client'

import { Suspense, useState, useRef, useCallback } from 'react'
import { Canvas, useFrame } from '@react-three/fiber'
import {
  OrbitControls,
  Environment,
  ContactShadows,
  Html,
  useGLTF,
} from '@react-three/drei'
import * as THREE from 'three'

/* ─── Types ─────────────────────────────────────── */

interface Annotation {
  position: [number, number, number]
  label: string
  detail: string
}

interface ProstheticViewerProps {
  /** Path to .glb model in /public. When null, renders placeholder geometry. */
  modelPath?: string | null
  annotations?: Annotation[]
  /** Height of the viewer container. Defaults to aspect-square. */
  className?: string
}

/* ─── Default annotations for the VA dental device ─ */

const DEFAULT_ANNOTATIONS: Annotation[] = [
  {
    position: [0.8, 0.6, 0.3],
    label: 'Palatal Framework',
    detail:
      'Custom-contoured titanium framework designed from CT scan data. Provides structural rigidity while minimizing tissue contact area.',
  },
  {
    position: [-0.6, 0.2, 0.7],
    label: 'Retention Clasps',
    detail:
      'Flexible clasp arms engage undercuts on remaining teeth. Designed for passive insertion with active retention.',
  },
  {
    position: [0.0, -0.3, 0.9],
    label: 'Denture Base',
    detail:
      'Biocompatible acrylic resin base seats against edentulous ridge. 3D-printed for precise fit to the veteran\'s anatomy.',
  },
  {
    position: [-0.2, 0.8, -0.3],
    label: 'Occlusal Surface',
    detail:
      'Prosthetic teeth positioned to restore functional occlusion. Material selected for wear resistance and natural appearance.',
  },
]

/* ─── Placeholder Dental Prosthetic Geometry ─────── */

function PlaceholderModel({
  wireframe,
  exploded,
}: {
  wireframe: boolean
  exploded: boolean
}) {
  const groupRef = useRef<THREE.Group>(null)

  useFrame((_, delta) => {
    if (groupRef.current) {
      groupRef.current.rotation.y += delta * 0.15
    }
  })

  const explodeOffset = exploded ? 0.5 : 0
  const materialProps = {
    transparent: true,
    opacity: wireframe ? 0.3 : 0.85,
    wireframe,
  }

  return (
    <group ref={groupRef} scale={1.2}>
      {/* Palatal arch (main body) */}
      <mesh position={[0, explodeOffset * 0.5, 0]}>
        <torusGeometry args={[0.7, 0.12, 16, 32, Math.PI]} />
        <meshStandardMaterial
          color="#c4a882"
          roughness={0.3}
          metalness={0.1}
          {...materialProps}
        />
      </mesh>

      {/* Framework base plate */}
      <mesh
        position={[0, -0.05 - explodeOffset * 0.3, 0]}
        rotation={[Math.PI / 2, 0, 0]}
      >
        <cylinderGeometry args={[0.55, 0.65, 0.06, 32, 1, false, 0, Math.PI]} />
        <meshStandardMaterial
          color="#8a8a8a"
          roughness={0.2}
          metalness={0.8}
          {...materialProps}
        />
      </mesh>

      {/* Prosthetic teeth row — left */}
      {Array.from({ length: 5 }).map((_, i) => {
        const angle = (i / 4) * Math.PI * 0.6 + Math.PI * 0.2
        const r = 0.62
        return (
          <mesh
            key={`tooth-l-${i}`}
            position={[
              Math.cos(angle) * r,
              0.15 + explodeOffset * 0.8,
              Math.sin(angle) * r * 0.4,
            ]}
            scale={[0.08 + i * 0.005, 0.12, 0.06]}
          >
            <boxGeometry args={[1, 1, 1]} />
            <meshStandardMaterial
              color="#f0ebe3"
              roughness={0.4}
              metalness={0.05}
              {...materialProps}
            />
          </mesh>
        )
      })}

      {/* Prosthetic teeth row — right */}
      {Array.from({ length: 5 }).map((_, i) => {
        const angle = Math.PI - ((i / 4) * Math.PI * 0.6 + Math.PI * 0.2)
        const r = 0.62
        return (
          <mesh
            key={`tooth-r-${i}`}
            position={[
              Math.cos(angle) * r,
              0.15 + explodeOffset * 0.8,
              Math.sin(angle) * r * 0.4,
            ]}
            scale={[0.08 + i * 0.005, 0.12, 0.06]}
          >
            <boxGeometry args={[1, 1, 1]} />
            <meshStandardMaterial
              color="#f0ebe3"
              roughness={0.4}
              metalness={0.05}
              {...materialProps}
            />
          </mesh>
        )
      })}

      {/* Retention clasps (left and right) */}
      {[-1, 1].map((side) => (
        <mesh
          key={`clasp-${side}`}
          position={[
            side * 0.75,
            0.05 - explodeOffset * 0.4,
            0.1,
          ]}
          rotation={[0, 0, side * 0.3]}
        >
          <torusGeometry args={[0.1, 0.02, 8, 16, Math.PI]} />
          <meshStandardMaterial
            color="#b0b0b0"
            roughness={0.15}
            metalness={0.9}
            {...materialProps}
          />
        </mesh>
      ))}

      {/* Connector bars */}
      {[-0.4, 0.4].map((x) => (
        <mesh
          key={`bar-${x}`}
          position={[x, -0.02, 0.15]}
          rotation={[Math.PI / 2, 0, 0]}
        >
          <cylinderGeometry args={[0.015, 0.015, 0.3, 8]} />
          <meshStandardMaterial
            color="#999"
            roughness={0.2}
            metalness={0.7}
            {...materialProps}
          />
        </mesh>
      ))}
    </group>
  )
}

/* ─── GLB Model Loader ──────────────────────────── */

function GLBModel({
  path,
  wireframe,
}: {
  path: string
  wireframe: boolean
}) {
  const { scene } = useGLTF(path)
  const groupRef = useRef<THREE.Group>(null)

  // Apply wireframe to all meshes
  scene.traverse((child) => {
    if (child instanceof THREE.Mesh && child.material) {
      const mat = child.material as THREE.MeshStandardMaterial
      mat.wireframe = wireframe
    }
  })

  useFrame((_, delta) => {
    if (groupRef.current) {
      groupRef.current.rotation.y += delta * 0.15
    }
  })

  return (
    <group ref={groupRef}>
      <primitive object={scene} scale={1} />
    </group>
  )
}

/* ─── Annotation Hotspots ────────────────────────── */

function AnnotationHotspot({
  annotation,
  index,
  active,
  onSelect,
}: {
  annotation: Annotation
  index: number
  active: boolean
  onSelect: (i: number | null) => void
}) {
  return (
    <Html
      position={annotation.position}
      center
      distanceFactor={4}
      zIndexRange={[10, 0]}
    >
      <button
        onClick={(e) => {
          e.stopPropagation()
          onSelect(active ? null : index)
        }}
        className={`
          w-6 h-6 rounded-full border-2 flex items-center justify-center
          text-[10px] font-mono font-bold cursor-pointer
          transition-all duration-200 select-none
          ${
            active
              ? 'bg-copper border-copper text-white scale-125'
              : 'bg-white/10 border-copper/60 text-copper hover:bg-copper/20 hover:scale-110'
          }
        `}
        style={{ backdropFilter: 'blur(8px)' }}
      >
        {index + 1}
      </button>
    </Html>
  )
}

/* ─── Loading Indicator ──────────────────────────── */

function LoadingFallback() {
  return (
    <Html center>
      <div className="flex flex-col items-center gap-3">
        <div className="w-8 h-8 border-2 border-copper/30 border-t-copper rounded-full animate-spin" />
        <span className="font-mono text-xs text-titanium">
          Loading model...
        </span>
      </div>
    </Html>
  )
}

/* ─── Main Viewer Component ──────────────────────── */

export default function ProstheticViewer({
  modelPath = null,
  annotations = DEFAULT_ANNOTATIONS,
  className = 'aspect-square',
}: ProstheticViewerProps) {
  const [wireframe, setWireframe] = useState(false)
  const [exploded, setExploded] = useState(false)
  const [activeAnnotation, setActiveAnnotation] = useState<number | null>(null)
  const [autoRotate, setAutoRotate] = useState(true)
  const controlsRef = useRef(null)

  const handleAnnotationSelect = useCallback((i: number | null) => {
    setActiveAnnotation(i)
    if (i !== null) setAutoRotate(false)
  }, [])

  const hasModel = modelPath !== null

  return (
    <div className={`relative rounded-xl overflow-hidden ${className}`}>
      {/* Three.js Canvas */}
      <Canvas
        camera={{ position: [0, 0.5, 3], fov: 45 }}
        dpr={[1, 2]}
        gl={{ antialias: true, alpha: true }}
        style={{ background: 'transparent' }}
      >
        <Suspense fallback={<LoadingFallback />}>
          {/* Lighting */}
          <ambientLight intensity={0.4} />
          <directionalLight position={[5, 5, 5]} intensity={0.8} />
          <directionalLight position={[-3, 3, -3]} intensity={0.3} />
          <pointLight position={[0, 2, 0]} intensity={0.3} color="#B87333" />

          {/* Model */}
          {hasModel ? (
            <GLBModel path={modelPath} wireframe={wireframe} />
          ) : (
            <PlaceholderModel wireframe={wireframe} exploded={exploded} />
          )}

          {/* Annotations */}
          {annotations.map((ann, i) => (
            <AnnotationHotspot
              key={i}
              annotation={ann}
              index={i}
              active={activeAnnotation === i}
              onSelect={handleAnnotationSelect}
            />
          ))}

          {/* Environment & Shadows */}
          <ContactShadows
            position={[0, -0.8, 0]}
            opacity={0.3}
            scale={4}
            blur={2}
          />
          <Environment preset="studio" />

          {/* Controls */}
          <OrbitControls
            ref={controlsRef}
            autoRotate={autoRotate && activeAnnotation === null}
            autoRotateSpeed={1}
            enablePan={false}
            minDistance={1.5}
            maxDistance={6}
            maxPolarAngle={Math.PI * 0.75}
          />
        </Suspense>
      </Canvas>

      {/* Control Bar */}
      <div className="absolute bottom-0 left-0 right-0 p-3 flex items-center justify-between bg-gradient-to-t from-slate-950/80 to-transparent">
        <div className="flex items-center gap-2">
          <ControlButton
            active={wireframe}
            onClick={() => setWireframe(!wireframe)}
            label="Wireframe"
          />
          {!hasModel && (
            <ControlButton
              active={exploded}
              onClick={() => setExploded(!exploded)}
              label="Exploded"
            />
          )}
          <ControlButton
            active={autoRotate}
            onClick={() => setAutoRotate(!autoRotate)}
            label="Rotate"
          />
        </div>

        {!hasModel && (
          <span className="font-mono text-[10px] text-titanium/40 hidden sm:block">
            Placeholder &mdash; drop .glb to activate
          </span>
        )}
      </div>

      {/* Annotation Detail Panel */}
      {activeAnnotation !== null && annotations[activeAnnotation] && (
        <div className="absolute top-3 right-3 w-64 glass rounded-xl p-4 animate-in fade-in slide-in-from-right-2">
          <div className="flex items-start justify-between gap-2 mb-2">
            <div className="flex items-center gap-2">
              <span className="w-5 h-5 rounded-full bg-copper text-white text-[10px] font-mono font-bold flex items-center justify-center flex-shrink-0">
                {activeAnnotation + 1}
              </span>
              <h4 className="font-serif text-sm text-white leading-tight">
                {annotations[activeAnnotation].label}
              </h4>
            </div>
            <button
              onClick={() => setActiveAnnotation(null)}
              className="text-titanium/40 hover:text-white text-lg leading-none flex-shrink-0"
            >
              &times;
            </button>
          </div>
          <p className="text-titanium text-xs leading-relaxed">
            {annotations[activeAnnotation].detail}
          </p>
        </div>
      )}
    </div>
  )
}

/* ─── Control Button ─────────────────────────────── */

function ControlButton({
  active,
  onClick,
  label,
}: {
  active: boolean
  onClick: () => void
  label: string
}) {
  return (
    <button
      onClick={onClick}
      className={`
        px-3 py-1.5 rounded-lg font-mono text-[11px] transition-all duration-200
        ${
          active
            ? 'bg-copper/20 text-copper border border-copper/30'
            : 'bg-white/5 text-titanium/60 border border-white/10 hover:bg-white/10 hover:text-titanium'
        }
      `}
    >
      {label}
    </button>
  )
}
